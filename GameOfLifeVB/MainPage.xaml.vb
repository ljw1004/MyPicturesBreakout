' This code is an adaption of the original at
' https://github.com/Microsoft/Win2D/blob/master/tests/ExampleGallery/Shared/GameOfLife.xaml.cs
' which is (c) Microsoft and licensed under the Apache License

Imports System.Numerics
Imports Microsoft.Graphics.Canvas
Imports Microsoft.Graphics.Canvas.Effects
Imports Microsoft.Graphics.Canvas.UI.Xaml
Imports Windows.UI

Public NotInheritable Class MainPage
    Inherits Page

    Const simulationW = 200
    Const simulationH = 100

    ' The simulation Is updated by swapping back And forth between two surfaces,
    ' repeatedly drawing the contents of one onto the other using an image effect
    ' that implements the rules of the cellular automaton.
    Dim currentSurface, nextSurface As CanvasRenderTarget

    Dim isPointerDown As Boolean
    Dim lastPointerX, lastPointerY As Integer

    Dim countNeighborsEffect As ConvolveMatrixEffect
    Dim liveOrDieEffect As DiscreteTransferEffect
    Dim invertEffect As LinearTransferEffect
    Dim transformEffect As Transform2DEffect

    Dim timer As Stopwatch = Stopwatch.StartNew()
    Dim lastTime As TimeSpan

    Protected Overrides Sub OnNavigatedFrom(e As NavigationEventArgs)
        ' Explicitly remove references to allow the Win2D controls to get garbage collected
        ' TODO: figure out when is the appropriate time to recreate it
        canvas.RemoveFromVisualTree()
        canvas = Nothing
    End Sub



    Sub Canvas_CreateResources(sender As CanvasControl, args As Object) Handles canvas.CreateResources
        Const defaultDpi = 96.0F

        currentSurface = New CanvasRenderTarget(sender, simulationW, simulationH, defaultDpi)
        nextSurface = New CanvasRenderTarget(sender, simulationW, simulationH, defaultDpi)

        CreateEffects()

        ' Initialize cells to random values.
        RandomizeSimulation(Nothing, Nothing)
    End Sub


    Sub Canvas_Draw(sender As CanvasControl, args As CanvasDrawEventArgs) Handles canvas.Draw
        ' Update the simulation state.
        Dim currentTime = timer.Elapsed
        Dim elapsed = (currentTime - lastTime).TotalSeconds
        If elapsed >= 1 / 30 Then
            UpdateSimulation()
            lastTime = currentTime
        End If

        ' Display the current surface.
        invertEffect.Source = currentSurface
        transformEffect.TransformMatrix = GetDisplayTransform(canvas.Size, canvas, simulationW, simulationH)
        args.DrawingSession.DrawImage(transformEffect)

        sender.Invalidate()
    End Sub

    Sub UpdateSimulation()
        ' Use the current surface as input.
        countNeighborsEffect.Source = currentSurface

        ' Draw it onto the next surface, using an image effect that implements the Game of Life cellular automaton.
        Using ds = nextSurface.CreateDrawingSession()
            ds.DrawImage(liveOrDieEffect)
        End Using

        ' Swap the current And next surfaces.
        Dim tmp = currentSurface
        currentSurface = nextSurface
        nextSurface = tmp
    End Sub

    Sub CreateEffects()
        ' The Game of Life Is a cellular automaton with very simple rules.
        ' Each cell (pixel) can be either alive (white) Or dead (black).
        ' The state Is updated by:
        '
        '  - for each cell, count how many of its 8 neighbors are alive
        '  - if less than two, the cell dies from loneliness
        '  - if exactly two, the cell keeps its current state
        '  - if exactly three, the cell become alive
        '  - if more than three, the cell dies from overcrowding

        ' Step 1: use a convolve matrix To count how many neighbors are alive. This filter
        ' also includes the state of the current cell, but with a lower weighting. The result
        ' Is an arithmetic encoding where (value / 2) indicates how many neighbors are alive,
        ' And (value % 2) Is the state of the cell itself. This Is divided by 18 to make it
        ' fit within 0-1 color range.

        countNeighborsEffect = New ConvolveMatrixEffect With {
            .KernelMatrix = {2.0F, 2.0F, 2.0F,
                             2.0F, 1.0F, 2.0F,
                             2.0F, 2.0F, 2.0F},
            .Divisor = 18,
            .BorderMode = EffectBorderMode.Hard
            }

        ' Step 2: use a color transfer table To map the different states produced by the
        ' convolve matrix to whether the cell should live Or die. Each pair of entries in
        ' this table corresponds to a certain number of live neighbors. The first of the
        ' pair Is the result if the current cell Is dead, Or the second if it Is alive.

        Dim transferTable =
            {
                0F, 0F,   ' 0 live neighbors -> dead cell
                0F, 0F,   ' 1 live neighbors -> dead cell
                0F, 1.0F,   ' 2 live neighbors -> cell keeps its current state
                1.0F, 1.0F,   ' 3 live neighbors -> live cell
                0F, 0F,   ' 4 live neighbors -> dead cell
                0F, 0F,   ' 5 live neighbors -> dead cell
                0F, 0F,   ' 6 live neighbors -> dead cell
                0F, 0F,   ' 7 live neighbors -> dead cell
                0F, 0F    ' 8 live neighbors -> dead cell
            }


        liveOrDieEffect = New DiscreteTransferEffect With {
            .Source = countNeighborsEffect,
            .RedTable = transferTable,
            .GreenTable = transferTable,
            .BlueTable = transferTable
        }

        ' Step 3: the algorithm Is implemented In terms Of white = live,
        ' black = dead, but we invert these colors before displaying the
        ' result, just 'cause I think it looks better that way.

        invertEffect = New LinearTransferEffect With
            {
                .RedSlope = -1,
                .RedOffset = 1,
                .GreenSlope = -1,
                .GreenOffset = 1,
                .BlueSlope = -1,
                .BlueOffset = 1
            }

        ' Step 4: insert our own DPI compensation effect To Stop the system trying To
        ' automatically convert DPI for us. The Game of Life simulation always works
        ' in pixels (96 DPI) regardless of display DPI. Normally, the system would
        ' handle this mismatch automatically And scale the image up as needed to fit
        ' higher DPI displays. We don't want that behavior here, because it would use
        ' a linear filter while we want nearest neighbor. So we insert a no-op DPI
        ' converter of our own. This overrides the default adjustment by telling the
        ' system the source image Is already the same DPI as the destination canvas
        ' (even though it really isn't). We'll handle any necessary scaling later
        ' ourselves, using Transform2DEffect to control the interpolation mode.

        Dim dpiCompensationEffect As New DpiCompensationEffect With
            {
                .Source = invertEffect,
                .SourceDpi = New Vector2(canvas.Dpi)
            }

        ' Step 5: a transform matrix scales up the simulation rendertarget And moves
        ' it to the right part of the screen. This uses nearest neighbor filtering
        ' to avoid unwanted blurring of the cell shapes.

        transformEffect = New Transform2DEffect With
            {
                .Source = dpiCompensationEffect,
                .InterpolationMode = CanvasImageInterpolation.NearestNeighbor
            }
    End Sub

    ' Initializes the simulation to a random state.
    Sub RandomizeSimulation(sender As Object, e As RoutedEventArgs)
        Dim random As New Random()
        Dim cols = New Color(simulationW * simulationH - 1) {}
        For i = 0 To cols.Length - 1
            cols(i) = If(random.NextDouble() < 0.25, Colors.White, Colors.Black)
        Next
        currentSurface.SetPixelColors(cols)
    End Sub

    Sub Canvas_PointerPressed(sender As Object, e As PointerRoutedEventArgs) Handles canvas.PointerPressed
        isPointerDown = True
        lastPointerX = Integer.MaxValue
        lastPointerY = Integer.MaxValue
        ProcessPointerInput(e)
    End Sub

    Sub Canvas_PointerReleased(sender As Object, e As PointerRoutedEventArgs) Handles canvas.PointerReleased
        isPointerDown = False
    End Sub

    Sub Canvas_PointerMoved(sender As Object, e As PointerRoutedEventArgs) Handles canvas.PointerMoved
        ProcessPointerInput(e)
    End Sub

    ' Toggles the color of cells when they are clicked on.
    Sub ProcessPointerInput(e As PointerRoutedEventArgs)
        If Not isPointerDown Then Return

        ' Invert the display transform, to convert pointer positions into simulation rendertarget space.
        Dim transform As Matrix3x2
        Matrix3x2.Invert(GetDisplayTransform(canvas.Size, canvas, simulationW, simulationH), transform)

        For Each point In e.GetIntermediatePoints(canvas)
            If Not point.IsInContact Then Continue For

            Dim pos = Vector2.Transform(point.Position.ToVector2(), transform)

            Dim x = canvas.ConvertDipsToPixels(pos.X)
            Dim y = canvas.ConvertDipsToPixels(pos.Y)

            ' If the point is within the bounds of the rendertarget, and not the same as the last point...
            If x < 0 OrElse y < 0 OrElse x >= simulationW OrElse y >= simulationH Then Continue For
            If x = lastPointerX OrElse y = lastPointerY Then Continue For

            ' Read the current color.
            Dim cellColor = currentSurface.GetPixelColors(x, y, 1, 1)

            ' Toggle the value.
            cellColor(0) = If(cellColor(0).R > 0, Colors.Black, Colors.White)

            ' Set the new color.
            currentSurface.SetPixelColors(cellColor, x, y, 1, 1)

            lastPointerX = x
            lastPointerY = y
        Next
    End Sub


    Public Function GetDisplayTransform(controlSize As Size, canvas As ICanvasResourceCreatorWithDpi, designWidth As Integer, designHeight As Integer) As Matrix3x2
        Dim sourceSize As New Vector2(canvas.ConvertPixelsToDips(designWidth), canvas.ConvertPixelsToDips(designHeight))
        Return GetDisplayTransform(controlSize.ToVector2(), sourceSize)
    End Function

    Public Function GetDisplayTransform(outputSize As Vector2, sourceSize As Vector2) As Matrix3x2
        ' Scale the display to fill the control.
        Dim scale = outputSize / sourceSize
        Dim offset = Vector2.Zero

        ' Letterbox Or pillarbox to preserve aspect ratio.
        If scale.X > scale.Y Then
            scale.X = scale.Y
            offset.X = (outputSize.X - sourceSize.X * scale.X) / 2
        Else
            scale.Y = scale.X
            offset.Y = (outputSize.Y - sourceSize.Y * scale.Y) / 2
        End If

        ' TODO #4479 once .NET Native x64 codegen bug Is fixed, change this back to:
        '          Return Matrix3x2.CreateScale(scale) *
        '                 Matrix3x2.CreateTranslation(offset);
        Return New Matrix3x2(scale.X, 0,
                             0, scale.Y,
                             offset.X, offset.Y)
    End Function



End Class
