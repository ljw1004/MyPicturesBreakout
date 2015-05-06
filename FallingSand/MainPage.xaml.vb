Imports System.Numerics
Imports Microsoft.Graphics.Canvas
Imports Microsoft.Graphics.Canvas.Effects
Imports Microsoft.Graphics.Canvas.UI.Xaml
Imports Windows.UI

Public NotInheritable Class MainPage
    Inherits Page

    Const CHEIGHT = 8
    Const CWIDTH = 8

    WithEvents canvas1 As CanvasControl
    Dim surfaceTruth1, surfaceTruth2 As CanvasRenderTarget
    Dim surfaceRender As CanvasRenderTarget

    Sub New()
        InitializeComponent()
        canvas1 = New CanvasControl With {.Width = 300, .Height = 300}
        container1.Children.Add(canvas1)
    End Sub


    Sub Canvas_CreateResources(sender As CanvasControl, args As Object) Handles canvas1.CreateResources
        Const defaultDpi = 96.0F
        surfaceTruth1 = New CanvasRenderTarget(canvas1, CWIDTH, CHEIGHT, defaultDpi)
        surfaceTruth2 = New CanvasRenderTarget(canvas1, CWIDTH, CHEIGHT, defaultDpi)
        surfaceRender = New CanvasRenderTarget(canvas1, CWIDTH, CHEIGHT, defaultDpi)
        surfaceTruth1.SetPixelColors(Enumerable.Repeat(Colors.Black, CWIDTH * CHEIGHT).ToArray, 0, 0, CWIDTH, CHEIGHT)

        surfaceTruth1.SetPixelColors({Colors.Red, Colors.Green, Colors.Blue, Colors.White}, 0, 0, 4, 1)
        surfaceTruth1.SetPixelColors({Colors.White, Colors.LightGray, Colors.DarkGray, Colors.DarkSlateBlue}, 0, 1, 4, 1)

        Using ds = surfaceRender.CreateDrawingSession()
            ds.DrawImage(surfaceTruth1)
        End Using

    End Sub

    Sub DoStep() Handles button1.Click


        Dim ingress = Function(k As KernelTracker) As Single
                          If k(Kernel.Center) = 1 Then Return 0
                          If k(Kernel.Center) = 0.5 Then Return 0
                          If k(Kernel.Center) = 0.5 AndAlso k(Kernel.Up) = 0.5 Then Return 0.5
                          If k(Kernel.Center) = 0.5 AndAlso k(Kernel.UpRight) = 0.5 AndAlso k(Kernel.Right) <> 0 Then Return 0.5
                          Return 0
                      End Function
        Dim egress = Function(k As KernelTracker) As Single
                         If k(Kernel.Center) = 1 Then Return 1
                         If k(Kernel.Center) = 0 Then Return 0
                         If k(Kernel.Center) = 0.5 AndAlso k(Kernel.Down) = 0 Then Return 0
                         If k(Kernel.Center) = 0.5 AndAlso k(Kernel.DownLeft) = 0 AndAlso k(Kernel.Left) <> 0.5 Then Return 0
                         Return 0.5
                     End Function

    End Sub

    Class KernelTracker
        Private _Matrix As Single()
        Private _Touched As Boolean()
        '
        Public ConvolveMatrix As Single()
        Public TransferTable As Single()

        Shared Function Generate(values As Single(), touches As Kernel(), lambda As Func(Of KernelTracker, Single)) As KernelTracker
            Dim k As New KernelTracker
            k._Touched = {False, False, False, False, False, False, False, False, False}
            k._Matrix = New Single(8) {}
            For i = 0 To IntPow(values.Length, touches.Length)
                Dim v = i
                For Each t In touches
                    k._Matrix(t) = values(v Mod values.Length)
                    v \= values.Length
                Next
                Dim r = lambda(k)
                ' TODO: finish it
            Next
        End Function

        Default Public ReadOnly Property Matrix(i As Kernel) As Single
            Get
                _Touched(i) = True
                Return _Matrix(i)
            End Get
        End Property
    End Class

    Enum Kernel
        UpLeft = 0
        Up = 1
        UpRight = 2
        Left = 3
        Center = 4
        Right = 5
        DownLeft = 6
        Down = 7
        DownRight = 8
    End Enum

    Sub Canvas1_Draw(sender As CanvasControl, args As CanvasDrawEventArgs) Handles canvas1.Draw
        Dim sourceSizeDips As New Vector2(canvas1.ConvertPixelsToDips(CWIDTH), canvas1.ConvertPixelsToDips(CHEIGHT))
        Dim canvasSizeDips As New Vector2(CSng(canvas1.ActualWidth), CSng(canvas1.ActualHeight))
        Dim scale = canvasSizeDips / sourceSizeDips

        Dim xform1 = Matrix3x2.CreateScale(scale)
        Dim dpi1 As New DpiCompensationEffect With {.Source = surfaceRender, .SourceDpi = New Vector2(canvas1.Dpi)}
        Dim xformEffect1 As New Transform2DEffect With {.Source = dpi1, .TransformMatrix = xform1, .InterpolationMode = CanvasImageInterpolation.NearestNeighbor}
        args.DrawingSession.DrawImage(xformEffect1)
    End Sub


    Sub Canvas_Pointer(sender As Object, e As PointerRoutedEventArgs) Handles canvas1.PointerPressed, canvas1.PointerMoved
        Dim sourceSizeDips As New Vector2(canvas1.ConvertPixelsToDips(CWIDTH), canvas1.ConvertPixelsToDips(CHEIGHT))
        Dim canvasSizeDips As New Vector2(CSng(canvas1.ActualWidth), CSng(canvas1.ActualHeight))
        Dim scale = canvasSizeDips.X / sourceSizeDips.X
        Dim canvasPointDips = e.GetCurrentPoint(canvas1).Position.ToVector2() / canvas1.ConvertPixelsToDips(1)
        Dim sourcePointDips = canvasPointDips / scale
        Dim x = CInt(Math.Floor(sourcePointDips.X))
        Dim y = CInt(Math.Floor(sourcePointDips.Y))
        If e.Pointer.IsInContact AndAlso x >= 0 AndAlso y >= 0 AndAlso x < CWIDTH AndAlso y < CHEIGHT Then
            'Dim pointerColor = If(e.GetCurrentPoint(canvas1).Properties.IsRightButtonPressed, pointerColorRight, pointerColorLeft)
            'App.pixels(y * CWIDTH + x) = pointerColor
            'surface1.SetPixelColors({pointerColor}, x, y, 1, 1)
            'canvas1.Invalidate()
        End If
    End Sub

End Class


Public Module Utils
    Public Function IntPow(x As Integer, y As Integer) As Integer
        Dim r = 1
        For i = 0 To y - 1
            r *= x
        Next
        Return r
    End Function
End Module