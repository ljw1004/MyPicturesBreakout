Imports Windows.Graphics.Imaging
Imports Windows.UI
Imports Windows.UI.Xaml.Shapes
Imports Windows.Devices.Input
Imports Windows.Storage
Imports System.Runtime.Serialization
Imports System.Text

<DataContract(Name:="GameData", [Namespace]:="")>
Public Class GameData
    <DataMember> Public MAXBALLS As Integer = 6
    <DataMember> Public MAXTARGETS As Integer = 15
    <DataMember> Public MAXBEEPS As Integer = 10
    <DataMember> Public NWIDTH As Integer = 400
    <DataMember> Public NHEIGHT As Integer = 400 * 5 \ 3
    <DataMember> Public NPADDLELONG As Integer = 100
    <DataMember> Public NPADDLESHORT As Integer = 30
    <DataMember> Public NBRICKWIDTH As Integer = 40
    <DataMember> Public NBRICKHEIGHT As Integer = 20
    <DataMember> Public NBALLSIZE As Integer = 20
    <DataMember> Public NLINEVELOCITY As Integer = 350
    <DataMember> Public NPADDLEVELOCITY As Integer = 900
    <DataMember> Public NGRAVITYX As Double = 0.0
    <DataMember> Public NGRAVITYY As Double = 10.0
    '
    <DataMember> Public NBRICKSX As Integer
    <DataMember> Public NBRICKSY As Integer
    <DataMember> Public NBRICKAREA As MyRect
    <DataMember> Public Balls As New ListOfBallInfo
    <DataMember(Name:="Bricks")> Private BricksProxy As BitmapSerializationProxy
    Public Bricks As BrickInfo(,)
    <DataMember> Public BrickCount As Integer
    <DataMember> Public DeadBrickCount As Integer
    Public BrickBitmap As MutableBitmapInfo ' pixel size of this bitmap is equal to the normalized size
    Public BackgroundBitmap As MutableBitmapInfo
    <DataMember(Name:="BrickBitmap")> Private BrickBitmapProxy As BitmapSerializationProxy
    <DataMember(Name:="BackgroundBitmap")> Private BackgroundBitmapProxy As BitmapSerializationProxy
    <DataMember> Public HasBorderLeft As Boolean
    <DataMember> Public HasBorderRight As Boolean
    <DataMember> Public HasBorderTop As Boolean
    <DataMember> Public HasBorderBottom As Boolean
    <DataMember> Public Targets As ListOfTargets
    <DataMember> Public TargetIndex As Integer
    '
    Public Canvas As Canvas
    Public CanvasBackground As Image
    Public CanvasForeground As Image
    Public BorderLeft, BorderRight, BorderTop, BorderBottom As Rectangle
    Public Paddles As List(Of PaddleInfo)
    Public SpareBalls As List(Of BallInfo)


    Public Sub Serialize(ByRef info As Byte(), ByRef bricks As Byte(), ByRef foreground As Byte(), ByRef background As Byte())
        BricksProxy = SerializeBricks()
        BrickBitmapProxy = BrickBitmap.Serialize()
        BackgroundBitmapProxy = BackgroundBitmap.Serialize()
        Using ms As New MemoryStream
            Dim d As New DataContractSerializer(GetType(GameData))
            d.WriteObject(ms, Me)
            info = ms.ToArray()
            bricks = BricksProxy.Content
            foreground = BrickBitmapProxy.Content
            background = BackgroundBitmapProxy.Content
        End Using
    End Sub

    Public Shared Function Deserialize(info As Byte(), bricks As Byte(), foreground As Byte(), background As Byte()) As GameData
        Dim dat As GameData
        Using ms As New MemoryStream(info)
            Dim d As New DataContractSerializer(GetType(GameData))
            dat = CType(d.ReadObject(ms), GameData)
        End Using
        dat.BricksProxy.Content = bricks
        dat.BrickBitmapProxy.Content = foreground
        dat.BackgroundBitmapProxy.Content = background
        dat.Bricks = DeserializeBricks(dat.BricksProxy) : dat.BricksProxy = Nothing
        dat.BrickBitmap = MutableBitmapInfo.Deserialize(dat.BrickBitmapProxy) : dat.BrickBitmapProxy = Nothing
        dat.BackgroundBitmap = MutableBitmapInfo.Deserialize(dat.BackgroundBitmapProxy) : dat.BackgroundBitmapProxy = Nothing
        GameLogic.InitializeCanvasForGameData(dat)
        Return dat
    End Function

    Public Function SerializeBricks() As BitmapSerializationProxy
        Dim p As New BitmapSerializationProxy With {.Width = NBRICKSX, .Height = NBRICKSY}
        Dim buf As New List(Of Byte)(p.Width * p.Height * 4)
        For y = 0 To p.Height - 1
            For x = 0 To p.Width - 1
                Dim b = Bricks(x, y)
                If b Is Nothing Then : buf.Add(0) : buf.Add(0) : buf.Add(0) : buf.Add(0)
                Else : buf.Add(255) : buf.Add(b.Color.R) : buf.Add(b.Color.G) : buf.Add(b.Color.B) : End If
            Next
        Next
        p.Content = buf.ToArray()
        Return p
    End Function

    Public Shared Function DeserializeBricks(p As BitmapSerializationProxy) As BrickInfo(,)
        Dim b = New BrickInfo(p.Width, p.Height) {}
        For y = 0 To p.Height - 1
            For x = 0 To p.Width - 1
                Dim i = (y * p.Width + x) * 4
                Dim c = Color.FromArgb(p.Content(i + 0), p.Content(i + 1), p.Content(i + 2), p.Content(i + 3))
                If c.A = 0 Then Continue For
                Dim bi As New BrickInfo With {.IX = x, .IY = y, .Color = c, .HP = 1, .MinMomentumToDestroy = 100, .MinMomentumToPassThrough = 5000}
                b(x, y) = bi
            Next
        Next
        Return b
    End Function

End Class

Public Enum Direction
    None
    Up
    Down
    Left
    Right
End Enum

<CollectionDataContract(Name:="Targets", Namespace:="")>
Public Class ListOfTargets : Inherits List(Of TargetInfo)
End Class

<DataContract(Name:="Target", [Namespace]:="")>
Public Structure TargetInfo
    Sub New(ix As Integer, iy As Integer)
        Me.IX = ix : Me.IY = iy
    End Sub
    <DataMember> Public IX As Integer
    <DataMember> Public IY As Integer
End Structure

<CollectionDataContract(Name:="Balls", Namespace:="")>
Public Class ListOfBallInfo : Inherits List(Of BallInfo)
End Class

<DataContract(Name:="Ball", [Namespace]:="")>
Public Class BallInfo
    Public objEllipse As Ellipse
    Public objLine As Line
    '
    <DataMember> Public nx As Double
    <DataMember> Public ny As Double
    <DataMember> Public ndx As Double
    <DataMember> Public ndy As Double
    <DataMember> Public IsBouncing As Boolean
    <DataMember> Public R As Byte
    <DataMember> Public G As Byte
    <DataMember> Public B As Byte
End Class

Public Class PaddleInfo
    Public objPaddle As Rectangle
    '
    Public Facing As Direction
    Public nPrevPos As MyRect
    Public nPos As MyRect
    Public ndx As Double
    Public ndy As Double
End Class

<DataContract(Name:="Rect", [Namespace]:="")>
Public Structure MyRect
    Sub New(left As Double, top As Double, width As Double, height As Double)
        Me.Left = left : Me.Top = top : Me.Width = width : Me.Height = height
    End Sub
    Function IsEmpty() As Boolean
        Return (Left = 0 AndAlso Top = 0 AndAlso Width = 0 AndAlso Height = 0)
    End Function
    '
    <DataMember> Public ReadOnly Left As Double
    <DataMember> Public ReadOnly Top As Double
    <DataMember> Public ReadOnly Width As Double
    <DataMember> Public ReadOnly Height As Double
    '
    Public ReadOnly Property Bottom As Double
        Get
            Return Top + Height
        End Get
    End Property
    Public ReadOnly Property Right As Double
        Get
            Return Left + Width
        End Get
    End Property
End Structure


Public Class BrickInfo
    Public IX As Integer
    Public IY As Integer
    Public Color As Color
    Public HP As Integer = 1
    Public MinMomentumToDestroy As Double = 100
    Public MinMomentumToPassThrough As Double = 5000
End Class


<DataContract(Name:="Bitmap", [Namespace]:="")>
Public Class BitmapSerializationProxy
    <DataMember> Public Width As Integer
    <DataMember> Public Height As Integer
    Public Content As Byte()
End Class


Public Module GameLogic
    ReadOnly RNG As New System.Random
    Const EPSILON = 0.0001F
    ReadOnly WHITE As New SolidColorBrush(Colors.White)
    ReadOnly GRAY As New SolidColorBrush(Colors.Gray)
    ReadOnly BLACK As New SolidColorBrush(Colors.Black)
    Const ZBACKGROUND = 1
    Const ZTRAILS = 2
    Const ZBRICKS = 3
    Const ZBALLS = 4
    Const ZPADDLE = 5
    Const ZBORDERS = 6

    ' How many beeps will we load for the game
    Const NUMBEEP = 20

    Public Async Function InitializeGameDataAsync(background As StorageFile, foreground As StorageFile) As Task(Of GameData)
        Dim dat As New GameData

        ' Bitmaps
        Dim backgroundBitmapTask = MutableBitmapInfo.LoadAndScaleBitmapAsync(background, dat.NWIDTH, dat.NHEIGHT, 0)
        Dim foregroundBitmapTask = MutableBitmapInfo.LoadAndScaleBitmapAsync(foreground, dat.NWIDTH, dat.NHEIGHT, dat.NBRICKHEIGHT)
        dat.BackgroundBitmap = Await backgroundBitmapTask.Log("backgroundBitmapTask")
        dat.BrickBitmap = Await foregroundBitmapTask.Log("foregroundBitmapTask")
        If dat.BackgroundBitmap Is Nothing OrElse dat.BrickBitmap Is Nothing Then Return Nothing
        dat.NBRICKSX = dat.NWIDTH \ dat.NBRICKWIDTH
        dat.NBRICKSY = dat.BrickBitmap.Height \ dat.NBRICKHEIGHT
        dat.NBRICKAREA = New MyRect(0, 0, dat.BrickBitmap.Width, dat.BrickBitmap.Height)

        ReDim dat.Bricks(dat.NBRICKSX, dat.NBRICKSY)
        dat.BrickCount = 0
        dat.DeadBrickCount = 0
        Using buf = dat.BrickBitmap.LockPixels()
            Dim NPIXELS_PER_BRICK = dat.NBRICKWIDTH * dat.NBRICKHEIGHT
            For iy = 0 To dat.NBRICKSY - 1
                For ix = 0 To dat.NBRICKSX - 1
                    Dim r = 0.0, g = 0.0, b = 0.0, a = 0.0
                    For py = 0 To dat.NBRICKHEIGHT - 1
                        For px = 0 To dat.NBRICKWIDTH - 1
                            Dim oldc = buf.Pixel(ix * dat.NBRICKWIDTH + px, iy * dat.NBRICKHEIGHT + py)
                            r += oldc.R : g += oldc.G : b += oldc.B : a += oldc.A
                        Next
                    Next
                    r /= NPIXELS_PER_BRICK : g /= NPIXELS_PER_BRICK : b /= NPIXELS_PER_BRICK : a /= NPIXELS_PER_BRICK
                    If a < 200 Then r = 0 : g = 0 : b = 0 : a = 0 Else a = 255
                    Dim newc = Color.FromArgb(CByte(a), CByte(r), CByte(g), CByte(b))
                    If a = 0 Then
                        dat.Bricks(ix, iy) = Nothing
                    Else
                        dat.Bricks(ix, iy) = New BrickInfo With {.IX = ix, .IY = iy, .Color = newc, .HP = 1}
                        dat.BrickCount += 1
                    End If
                    If a = 0 Then
                        For py = 0 To dat.NBRICKHEIGHT - 1
                            For px = 0 To dat.NBRICKWIDTH - 1
                                buf.Pixel(ix * dat.NBRICKWIDTH + px, iy * dat.NBRICKHEIGHT + py) = newc
                            Next
                        Next
                    Else
                        For py = 0 To dat.NBRICKHEIGHT - 1
                            buf.Pixel(ix * dat.NBRICKWIDTH + dat.NBRICKWIDTH - 1, iy * dat.NBRICKHEIGHT + py) = Colors.Black
                        Next
                        For px = 0 To dat.NBRICKWIDTH - 1
                            buf.Pixel(ix * dat.NBRICKWIDTH + px, iy * dat.NBRICKHEIGHT + dat.NBRICKHEIGHT - 1) = Colors.Black
                        Next
                    End If
                Next
            Next
        End Using

        ' Borders around the edges
        dat.HasBorderLeft = True : dat.HasBorderRight = True : dat.HasBorderTop = True : dat.HasBorderBottom = False

        ' One ball...
        Dim nbx = dat.NWIDTH / 2
        Dim nby = dat.NHEIGHT * 0.9
        Dim ndx = dat.NLINEVELOCITY * 0.5 * (RNG.NextDouble() - 0.5)
        Dim ndy = -dat.NLINEVELOCITY * (RNG.NextDouble() + 1.0) / 2.0
        dat.Balls.Add(New BallInfo With {.nx = nbx, .ny = nby, .ndx = ndx, .ndy = ndy, .R = 0, .G = 0, .B = 0})

        InitializeCanvasForGameData(dat)

        Return dat
    End Function

    Sub InitializeCanvasForGameData(dat As GameData)
        dat.Paddles = New List(Of PaddleInfo)
        dat.SpareBalls = New List(Of BallInfo)
        dat.Canvas = New Canvas() With {.Background = New SolidColorBrush(Colors.Pink)}
        '
        dat.CanvasBackground = New Image With {.Source = dat.BackgroundBitmap.Bitmap, .Stretch = Stretch.UniformToFill}
        dat.Canvas.AddAt(dat.CanvasBackground, 0, 0, ZBACKGROUND)
        dat.CanvasForeground = New Image With {.Source = dat.BrickBitmap.Bitmap, .Stretch = Stretch.Fill}
        dat.Canvas.AddAt(dat.CanvasForeground, 0, 0, ZBRICKS)
        '
        dat.BorderLeft = New Rectangle With {.Width = 1, .Fill = GRAY} : dat.Canvas.AddAt(dat.BorderLeft, 0, 0, ZBORDERS)
        dat.BorderRight = New Rectangle With {.Width = 1, .Fill = GRAY} : dat.Canvas.AddAt(dat.BorderRight, 0, 0, ZBORDERS)
        dat.BorderTop = New Rectangle With {.Height = 1, .Fill = GRAY} : dat.Canvas.AddAt(dat.BorderTop, 0, 0, ZBORDERS)
        '
        For i = 0 To dat.Balls.Count - 1
            Dim c = dat.Balls(i)
            Dim objEllipse As New Ellipse With {.Fill = New SolidColorBrush(Color.FromArgb(255, c.R, c.G, c.B))}
            dat.Balls(i).objEllipse = objEllipse
            dat.Canvas.AddAt(objEllipse, 0, 0, ZBALLS)
            Dim objLine As New Line With {.StrokeThickness = 2, .Stroke = GRAY}
            dat.Balls(i).objLine = objLine
            Canvas.SetZIndex(objLine, ZTRAILS)
            dat.Canvas.Children.Add(objLine)
        Next
        For i = dat.Balls.Count To dat.MAXBALLS - 1
            Dim objEllipse As New Ellipse With {.Visibility = Visibility.Collapsed}
            dat.Canvas.AddAt(objEllipse, 0, 0, ZBALLS)
            Dim objLine As New Line With {.StrokeThickness = 2, .Stroke = GRAY}
            Canvas.SetZIndex(objLine, ZTRAILS)
            dat.Canvas.Children.Add(objLine)
            dat.SpareBalls.Add(New BallInfo With {.objEllipse = objEllipse, .objLine = objLine})
        Next
    End Sub

    Sub Resize(dat As GameData, canvasWidth As Integer, canvasHeight As Integer)
        dat.Canvas.Width = canvasWidth
        dat.Canvas.Height = canvasHeight
        '
        dat.CanvasBackground.Width = canvasWidth
        dat.CanvasBackground.Height = canvasHeight
        dat.CanvasForeground.Width = canvasWidth * dat.NBRICKAREA.Width / dat.NWIDTH
        dat.CanvasForeground.Height = canvasHeight * dat.NBRICKAREA.Height / dat.NHEIGHT
        '
        If dat.HasBorderLeft Then dat.BorderLeft.Height = canvasHeight
        If dat.HasBorderTop Then dat.BorderTop.Width = canvasWidth
        If dat.HasBorderRight Then dat.BorderRight.Height = canvasHeight : Canvas.SetLeft(dat.BorderRight, canvasWidth - 1)
        If dat.HasBorderBottom Then dat.BorderBottom.Width = canvasWidth : Canvas.SetTop(dat.BorderBottom, canvasHeight - 1)
        '
        For Each pi In dat.Paddles
            ResizePaddle(pi, dat)
        Next
        '
        For Each bi In dat.Balls
            ResizeBall(bi, dat)
        Next
    End Sub

    Sub ResizePaddle(pi As PaddleInfo, dat As GameData)
        pi.objPaddle.Width = pi.nPos.Width * dat.Canvas.Width / dat.NWIDTH
        pi.objPaddle.Height = pi.nPos.Height * dat.Canvas.Height / dat.NHEIGHT
        Canvas.SetLeft(pi.objPaddle, pi.nPos.Left * dat.Canvas.Width / dat.NWIDTH)
        Canvas.SetTop(pi.objPaddle, pi.nPos.Top * dat.Canvas.Height / dat.NHEIGHT)
    End Sub

    Sub ResizeBall(bi As BallInfo, dat As GameData)
        bi.objEllipse.Width = dat.NBALLSIZE * dat.Canvas.Width / dat.NWIDTH
        bi.objEllipse.Height = dat.NBALLSIZE * dat.Canvas.Height / dat.NHEIGHT
        Canvas.SetLeft(bi.objEllipse, bi.nx * dat.Canvas.Width / dat.NWIDTH - bi.objEllipse.Width / 2)
        Canvas.SetTop(bi.objEllipse, bi.ny * dat.Canvas.Height / dat.NHEIGHT - bi.objEllipse.Height / 2)
        bi.objLine.X1 = bi.objEllipse.Center.X
        bi.objLine.Y1 = bi.objEllipse.Center.Y
        bi.objLine.X2 = bi.objLine.X1 - bi.ndx * 0.2 * dat.Canvas.Width / dat.NWIDTH
        bi.objLine.Y2 = bi.objLine.Y1 - bi.ndy * 0.2 * dat.Canvas.Height / dat.NHEIGHT
    End Sub

    Public Sub UpdateBalls(dat As GameData, tinterval As TimeSpan, ByRef wantPaddleBeep As Boolean, ByRef wantBlockBeep As Boolean, ByRef wantGuidedBeep As Boolean, ByRef ballsLost As ULong, ByRef bricksDestroyed As ULong)
        Dim now = DateTime.Now
        Dim interval = tinterval.TotalSeconds
        If interval > 0.05 Then interval = 0.05

        wantPaddleBeep = False
        wantBlockBeep = False
        wantGuidedBeep = False
        bricksDestroyed = 0
        ballsLost = 0

        ' We need to know where the paddle has moved in this time
        For Each pi In dat.Paddles
            Dim oldpos = pi.nPrevPos
            Dim newpos = pi.nPos
            pi.nPrevPos = newpos
            pi.ndx = (newpos.Left + newpos.Width / 2) - (oldpos.Left + oldpos.Width / 2)
            pi.ndy = (newpos.Top + newpos.Height / 2) - (oldpos.Top + oldpos.Height / 2)
        Next

        Dim deadbricks As New List(Of BrickInfo)
        Dim deadballs As New List(Of BallInfo)

        For Each bi In dat.Balls
            bi.IsBouncing = False

            Dim bhit = False
            Dim ox = bi.nx
            Dim oy = bi.ny
            ' compute new pos,vel
            bi.ndx += dat.NGRAVITYX * interval
            bi.ndy += dat.NGRAVITYY * interval
            Dim ndd = Math.Sqrt(bi.ndx * bi.ndx + bi.ndy * bi.ndy)
            If ndd > dat.NLINEVELOCITY Then bi.ndx = bi.ndx * dat.NLINEVELOCITY / ndd : bi.ndy = bi.ndy * dat.NLINEVELOCITY / ndd
            Dim nx = ox + bi.ndx * interval
            Dim ny = oy + bi.ndy * interval

            ' See if it hit a paddle
            For Each pi In dat.Paddles
                If dat.DeadBrickCount = 0 Then Exit For
                Dim frac = 0.0
                Dim hit = DoesLineEnterRect(ox, oy, nx, ny, pi.nPos, pi.ndx, pi.ndy, frac)
                If Not bi.IsBouncing AndAlso hit <> Direction.None Then
                    If frac > 0.3 AndAlso frac < 0.7 Then frac = 0 Else frac = (frac - 0.5) * dat.NLINEVELOCITY * 0.8
                    If hit = Direction.Left Then : bi.ndx = -Math.Abs(bi.ndx) + pi.ndx
                    ElseIf hit = Direction.Right Then : bi.ndx = Math.Abs(bi.ndx) + pi.ndx
                    Else : bi.ndx = bi.ndx + pi.ndx + frac : End If
                    bi.ndy = -Math.Abs(bi.ndy) + pi.ndy
                End If
                bhit = bhit OrElse (hit <> Direction.None)
            Next
            If SharedApp.Current.HasGuidanceSystem() AndAlso bhit AndAlso dat.Targets IsNot Nothing AndAlso dat.Targets.Count > 0 Then
                If dat.TargetIndex >= dat.Targets.Count Then dat.TargetIndex = 0
                Dim target As TargetInfo = dat.Targets(dat.TargetIndex)
                dat.TargetIndex += 1 : If dat.TargetIndex >= dat.Targets.Count Then dat.TargetIndex = 0
                Dim targetnx = dat.NBRICKAREA.Left + dat.NBRICKWIDTH * target.IX + dat.NBRICKWIDTH \ 2
                Dim targetny = dat.NBRICKAREA.Top + dat.NBRICKHEIGHT * target.IY + dat.NBRICKHEIGHT \ 2
                bi.ndx = targetnx - ox
                bi.ndy = targetny - oy
                ndd = Math.Sqrt(bi.ndx * bi.ndx + bi.ndy * bi.ndy)
                bi.ndx = bi.ndx * dat.NLINEVELOCITY / ndd : bi.ndy = bi.ndy * dat.NLINEVELOCITY / ndd
                nx = ox + bi.ndx * interval
                ny = oy + bi.ndy * interval
                wantGuidedBeep = True
            ElseIf bhit Then
                wantPaddleBeep = True
            End If

            ' See if it hit a brick. It adds to the fun if we don't do line-intersect test to remove all the balls in its path
            Dim hitix = CInt(Math.Floor((nx - dat.NBRICKAREA.Left) / dat.NBRICKWIDTH))
            Dim hitiy = CInt(Math.Floor((ny - dat.NBRICKAREA.Top) / dat.NBRICKHEIGHT))
            If hitix >= 0 AndAlso hitiy >= 0 AndAlso hitix < dat.NBRICKSX AndAlso hitiy < dat.NBRICKSY AndAlso dat.Bricks(hitix, hitiy) IsNot Nothing Then
                Dim ri = dat.Bricks(hitix, hitiy)
                Dim velocity = Math.Sqrt(bi.ndx * bi.ndx + bi.ndy * bi.ndy)
                If velocity > ri.MinMomentumToDestroy Then
                    ri.HP -= 1
                    If ri.HP <= 0 Then
                        bricksDestroyed += 1UL
                        deadbricks.Add(ri)
                        dat.Bricks(hitix, hitiy) = Nothing
                        dat.BrickCount -= 1
                        dat.DeadBrickCount += 1
                        wantBlockBeep = True
                        If dat.Targets IsNot Nothing Then
                            Dim wasTarget = dat.Targets.FindIndex(Function(t) t.IX = ri.IX AndAlso t.IY = ri.IY)
                            If wasTarget <> -1 AndAlso wasTarget < dat.TargetIndex Then dat.TargetIndex -= 1
                            If wasTarget <> -1 Then dat.Targets.RemoveAt(wasTarget)
                        End If
                    End If
                End If
                If velocity < ri.MinMomentumToPassThrough Then
                    Dim r = New MyRect(dat.NBRICKAREA.Left + hitix * dat.NBRICKWIDTH, dat.NBRICKAREA.Top + hitiy * dat.NBRICKHEIGHT, dat.NBRICKWIDTH, dat.NBRICKHEIGHT)
                    Dim hit = DoesLineEnterRect(ox, oy, nx, ny, r, 0, 0, 0)
                    If Not bi.IsBouncing AndAlso Not bhit AndAlso hit <> Direction.None Then
                        If hit = Direction.Left Then : bi.ndx = -Math.Abs(bi.ndx)
                        ElseIf hit = Direction.Right Then : bi.ndx = Math.Abs(bi.ndx)
                        ElseIf hit = Direction.Up Then : bi.ndy = -Math.Abs(bi.ndy)
                        ElseIf hit = Direction.Down Then : bi.ndy = Math.Abs(bi.ndy)
                        End If
                    End If
                    bhit = bhit OrElse (hit <> Direction.None)
                End If
            End If

            ' Clip border
            Dim w2 = dat.NBALLSIZE / 2, h2 = dat.NBALLSIZE / 2
            Dim isdead = False
            If nx - w2 < 0 AndAlso dat.HasBorderLeft Then nx = w2 : bi.ndx = Math.Abs(bi.ndx)
            If nx - w2 < 0 AndAlso dat.BorderLeft Is Nothing Then isdead = True
            If nx + w2 > dat.NWIDTH AndAlso dat.HasBorderRight Then nx = dat.NWIDTH - w2 : bi.ndx = -Math.Abs(bi.ndx)
            If nx + w2 > dat.NWIDTH AndAlso dat.BorderRight Is Nothing Then isdead = True
            If ny - h2 < 0 AndAlso dat.HasBorderTop Then ny = h2 : bi.ndy = Math.Abs(bi.ndy)
            If ny - h2 < 0 AndAlso dat.BorderTop Is Nothing Then isdead = True
            If ny + h2 > dat.NHEIGHT AndAlso dat.HasBorderBottom Then ny = dat.NHEIGHT - h2 : bi.ndy = -Math.Abs(bi.ndy)
            If ny + h2 > dat.NHEIGHT AndAlso dat.BorderBottom Is Nothing Then isdead = True
            If isdead Then deadballs.Add(bi) : ballsLost += 1UL
            bi.IsBouncing = bhit

            ' Update position
            bi.nx = nx
            bi.ny = ny
            ResizeBall(bi, dat)
        Next

        For Each bi In deadballs
            bi.objEllipse.Visibility = Visibility.Collapsed
            bi.objLine.Visibility = Visibility.Collapsed
            dat.Balls.Remove(bi)
            dat.SpareBalls.Add(bi)
        Next

        For Each ri In deadbricks
            ' Direction it should fall in? Based on criteria (1) Brick-free, (2) Border-free, (3) Gravity
            Dim allCandidates As New List(Of Direction) From {Direction.Up, Direction.Down, Direction.Left, Direction.Right}
            Dim candidates = allCandidates
            If candidates.Count > 1 Then
                If dat.HasBorderRight AndAlso dat.HasBorderLeft Then candidates.Remove(Direction.Left) : candidates.Remove(Direction.Right)
                If dat.HasBorderTop AndAlso dat.HasBorderBottom Then candidates.Remove(Direction.Up) : candidates.Remove(Direction.Down)
                If candidates.Count = 0 Then candidates = New List(Of Direction) From {Direction.Up, Direction.Down, Direction.Left, Direction.Right}
            End If
            If candidates.Count > 1 Then
                If ri.IX = dat.NBRICKSX - 1 OrElse dat.Bricks(ri.IX + 1, ri.IY) IsNot Nothing Then candidates.Remove(Direction.Right)
                If ri.IX = 0 OrElse dat.Bricks(ri.IX - 1, ri.IY) IsNot Nothing Then candidates.Remove(Direction.Left)
                If ri.IY = dat.NBRICKSY - 1 OrElse dat.Bricks(ri.IX, ri.IY + 1) IsNot Nothing Then candidates.Remove(Direction.Down)
                If ri.IY = 0 OrElse dat.Bricks(ri.IX, ri.IY - 1) IsNot Nothing Then candidates.Remove(Direction.Up)
                If candidates.Count = 0 Then candidates = New List(Of Direction) From {Direction.Up, Direction.Down, Direction.Left, Direction.Right}
            End If
            If candidates.Count > 1 Then
                If dat.HasBorderRight Then candidates.Remove(Direction.Right)
                If dat.HasBorderLeft Then candidates.Remove(Direction.Left)
                If dat.HasBorderBottom Then candidates.Remove(Direction.Down)
                If dat.HasBorderTop Then candidates.Remove(Direction.Up)
                If candidates.Count = 0 Then candidates = New List(Of Direction) From {Direction.Up, Direction.Down, Direction.Left, Direction.Right}
            End If
            If candidates.Count > 1 Then
                If dat.NGRAVITYY > 0 Then candidates.Remove(Direction.Up)
                If dat.NGRAVITYY < 0 Then candidates.Remove(Direction.Down)
                If dat.NGRAVITYX > 0 Then candidates.Remove(Direction.Left)
                If dat.NGRAVITYX < 0 Then candidates.Remove(Direction.Right)
                If candidates.Count = 0 Then candidates = New List(Of Direction) From {Direction.Up, Direction.Down, Direction.Left, Direction.Right}
            End If
            Dim winner = candidates(RNG.Next() Mod candidates.Count)
            Dim ndminor = dat.NLINEVELOCITY * 0.5 * (RNG.NextDouble() - 0.5)
            Dim ndmajor = dat.NLINEVELOCITY * (RNG.NextDouble() + 1.0) / 2.0
            Dim ndx, ndy As Double
            If winner = Direction.Up Then : ndx = ndminor : ndy = -ndmajor
            ElseIf winner = Direction.Down Then : ndx = ndminor : ndy = ndmajor
            ElseIf winner = Direction.Left Then : ndx = -ndmajor : ndy = ndminor
            ElseIf winner = Direction.Right Then : ndx = ndmajor : ndy = ndminor
            Else : ndx = ndminor : ndy = ndmajor
            End If
            ' Erase the old brick
            Dim newc = Color.FromArgb(0, 0, 0, 0)
            Using buf = dat.BrickBitmap.LockPixels()
                For py = 0 To dat.NBRICKHEIGHT - 1
                    For px = 0 To dat.NBRICKWIDTH - 1
                        buf.Pixel(ri.IX * dat.NBRICKWIDTH + px, ri.IY * dat.NBRICKHEIGHT + py) = newc
                    Next
                Next
            End Using
            ' Create the ball
            If dat.SpareBalls.Count > 0 Then
                Dim bi = dat.SpareBalls(dat.SpareBalls.Count - 1) : dat.SpareBalls.RemoveAt(dat.SpareBalls.Count - 1)
                bi.ndx = ndx : bi.ndy = ndy
                bi.nx = dat.NBRICKAREA.Left + ri.IX * dat.NBRICKWIDTH + dat.NBRICKWIDTH / 2 - dat.NBALLSIZE / 2
                bi.ny = dat.NBRICKAREA.Top + ri.IY * dat.NBRICKHEIGHT + dat.NBRICKHEIGHT / 2 - dat.NBRICKHEIGHT / 2
                bi.R = ri.Color.R : bi.G = ri.Color.G : bi.B = ri.Color.B
                bi.objEllipse.Fill = New SolidColorBrush(ri.Color)
                bi.objEllipse.Visibility = Visibility.Visible
                bi.objLine.Visibility = Visibility.Visible
                ResizeBall(bi, dat)
                dat.Balls.Add(bi)
            End If
        Next

        If dat.BrickCount <= dat.MAXTARGETS AndAlso dat.Targets Is Nothing Then
            dat.Targets = New ListOfTargets() With {.Capacity = dat.BrickCount}
            For iy = dat.NBRICKSY - 1 To 0 Step -1
                For dix = 0 To dat.NBRICKSX \ 2
                    Dim ix = dat.NBRICKSX \ 2 + dix
                    If ix < dat.NBRICKSX AndAlso dat.Bricks(ix, iy) IsNot Nothing Then dat.Targets.Add(New TargetInfo(ix, iy))
                    ix = dat.NBRICKSX \ 2 - 1 - dix
                    If ix >= 0 AndAlso dat.Bricks(ix, iy) IsNot Nothing Then dat.Targets.Add(New TargetInfo(ix, iy))
                Next
            Next
        End If
    End Sub

    Sub HandlePointers(dat As GameData, pointers As Dictionary(Of UInteger, Point))
        ' Make sure we have the right number of paddles on screen
        Dim numPaddles = pointers.Count 'If(pointers.Count = 0, 1, pointers.Count)
        While dat.Paddles.Count < numPaddles
            Dim rect As New Rectangle With {.Fill = WHITE}
            dat.Canvas.AddAt(rect, 0, 0, ZPADDLE)
            dat.Paddles.Add(New PaddleInfo With {.Facing = Direction.Up, .objPaddle = rect, .nPrevPos = New MyRect(0, 0, 0, 0)})
        End While
        While dat.Paddles.Count > numPaddles
            Dim i = dat.Paddles.Count - 1
            dat.Canvas.Children.Remove(dat.Paddles(i).objPaddle)
            dat.Paddles.RemoveAt(i)
        End While
        ' If you have more fingers on screen, then each paddle is a little smaller
        Dim ntotalWidth = 0.0
        For ii = 1 To pointers.Count
            ntotalWidth += 1 / ii
        Next
        Dim nw = dat.NPADDLELONG * ntotalWidth / pointers.Count
        Dim nh = dat.NPADDLESHORT
        ' Let's position the paddles. The bottom center of the paddle should be ~1/3" (40 canvas units) above the finger press
        For i = 0 To pointers.Count - 1
            Dim pi = dat.Paddles(i)
            Dim nx = pointers.Values(i).X * dat.NWIDTH / dat.Canvas.Width - nw / 2
            Dim ny = pointers.Values(i).Y * dat.NHEIGHT / dat.Canvas.Height - nh / 2
            If nx < 0 Then nx = 0
            If nx + nw > dat.NWIDTH Then nx = dat.NWIDTH - nw
            If ny < 0 Then ny = 0
            If ny + nh > dat.NHEIGHT Then ny = dat.NHEIGHT - nh
            pi.nPos = New MyRect(nx, ny, nw, nh)
            If pi.nPrevPos.IsEmpty() Then pi.nPrevPos = pi.nPos
            ResizePaddle(pi, dat)
        Next
    End Sub



    Function DoesLineEnterRect(x1 As Double, y1 As Double, x2 As Double, y2 As Double, r As MyRect, rdx As Double, rdy As Double, ByRef frac As Double) As Direction
        frac = 0
        ' Adjust relative frame of reference so it's as if the rectangle hasn't moved
        x1 = x1 + rdx : y1 = y1 + rdy
        ' Is it completely outside or inside the box? cheap tests...
        If x1 < r.Left AndAlso x2 < r.Left Then Return Direction.None
        If x1 > r.Right AndAlso x2 > r.Right Then Return Direction.None
        If y1 < r.Top AndAlso y2 < r.Top Then Return Direction.None
        If y1 > r.Bottom AndAlso y2 > r.Bottom Then Return Direction.None
        ' Is it going exactly horizontal or vertical? 
        If Math.Abs(x1 - x2) < EPSILON Then
            frac = (x1 - r.Left) / (r.Right - r.Left)
            If y1 < r.Top Then Return Direction.Up
            If y1 > r.Bottom Then Return Direction.Down
            Return Direction.None
        End If
        If Math.Abs(y1 - y2) < EPSILON Then
            frac = (y1 - r.Top) / (r.Bottom - r.Top)
            If x1 < r.Left Then Return Direction.Left
            If x1 > r.Right Then Return Direction.Right
            Return Direction.None
        End If
        ' Let's find which edges it actually intersects
        Dim htop = False, hbot = False, hleft = False, hright = False
        Dim xtop, xbot, yleft, yright As Double
        Dim dx = x2 - x1, dy = y2 - y1
        Dim ftop = (r.Top - y1) / dy
        Dim fbot = (r.Bottom - y1) / dy
        Dim fleft = (r.Left - x1) / dx
        Dim fright = (r.Right - x1) / dx
        If ftop > 0 AndAlso ftop < 1 Then
            xtop = (x1 + ftop * dx - r.Left) / (r.Right - r.Left)
            If xtop > 0 AndAlso xtop < 1 Then htop = True
        End If
        If fbot > 0 AndAlso fbot < 1 Then
            xbot = (x1 + fbot * dx - r.Left) / (r.Right - r.Left)
            If xbot > 0 AndAlso xbot < 1 Then hbot = True
        End If
        If fleft > 0 AndAlso fleft < 1 Then
            yleft = (y1 + fleft * dy - r.Top) / (r.Bottom - r.Top)
            If yleft > 0 AndAlso yleft < 1 Then hleft = True
        End If
        If fright > 0 AndAlso fright < 1 Then
            yright = (y1 + fright * dy - r.Top) / (r.Bottom - r.Top)
            If yright > 0 AndAlso yright < 1 Then hright = True
        End If
        ' That makes the tests easier
        If dy > 0 Then
            If htop Then : frac = xtop : Return Direction.Up
            ElseIf hleft Then : frac = yleft : Return Direction.Left
            ElseIf hright Then : frac = yright : Return Direction.Right
            Else : frac = 0 : Return Direction.None : End If
        ElseIf dy < 0 Then
            If hbot Then : frac = xbot : Return Direction.Down
            ElseIf hleft Then : frac = yleft : Return Direction.Left
            ElseIf hright Then : frac = yright : Return Direction.Right
            Else : frac = 0 : Return Direction.None : End If
        ElseIf dx > 0 Then
            If hleft Then : frac = yleft : Return Direction.Left
            ElseIf htop Then : frac = xtop : Return Direction.Up
            ElseIf hbot Then : frac = xbot : Return Direction.Down
            Else : frac = 0 : Return Direction.None : End If
        ElseIf dx < 0 Then
            If hright Then : frac = yright : Return Direction.Right
            ElseIf htop Then : frac = xtop : Return Direction.Up
            ElseIf hbot Then : frac = xbot : Return Direction.Down
            Else : frac = 0 : Return Direction.None : End If
        Else
            frac = 0 : Return Direction.None
        End If
    End Function

    Class ReadonlyBitmapInfo
        Public Width As Integer
        Public Height As Integer
        Public Buffer As Byte()

        Public Const RED = 0, GREEN = 1, BLUE = 2, ALPHA = 3
        ReadOnly Property CalcOffset(x As Integer, y As Integer) As Integer
            Get
                Return y * Width * 4 + x * 4
            End Get
        End Property
        ReadOnly Property Pixel(x As Integer, y As Integer) As Color
            Get
                Dim i = CalcOffset(x, y)
                Return Color.FromArgb(Buffer(i + ALPHA), Buffer(i + RED), Buffer(i + GREEN), Buffer(i + BLUE))
            End Get
        End Property
    End Class

    Class MutableBitmapInfo

        Private _bitmap As WriteableBitmap

        Shared Async Function LoadAndScaleBitmapAsync(file As StorageFile, NWIDTH As Integer, NHEIGHT As Integer, NBRICKHEIGHT As Integer) As Task(Of MutableBitmapInfo)
            Try
                Dim stream As Streams.IRandomAccessStream = Await file.OpenAsync(FileAccessMode.Read).Log("file.OpenAsync", file.Path & ":Read")
                Dim decoder As BitmapDecoder = Await BitmapDecoder.CreateAsync(stream).Log("BitmapDecoder.CreateAsync")
                Dim frame As BitmapFrame = Await decoder.GetFrameAsync(0).Log("GetFrameAsync", 0)
                Dim owidth = CInt(decoder.OrientedPixelWidth), oheight = CInt(decoder.OrientedPixelHeight)
                Dim isflipped = (decoder.OrientedPixelWidth = decoder.PixelHeight AndAlso decoder.OrientedPixelHeight = decoder.PixelWidth)
                ' For bricks (NBRICKHEIGHT <> 0) we scale to width=NWIDTH, and use whatever height the image proportions say
                ' For background (NBRICKHEIGHT = 0) we pick whichever scale is at least as big as NWIDTH*NHEIGHT
                Dim width = NWIDTH, height = oheight * width \ owidth
                If NBRICKHEIGHT = 0 AndAlso height < NHEIGHT Then
                    height = NHEIGHT : width = owidth * height \ oheight
                End If
                Dim xform As New BitmapTransform With {
                    .InterpolationMode = BitmapInterpolationMode.NearestNeighbor,
                    .ScaledWidth = CUInt(If(isflipped, height, width)),
                    .ScaledHeight = CUInt(If(isflipped, width, height)),
                    .Bounds = New BitmapBounds With {.X = 0, .Y = 0, .Width = CUInt(width), .Height = CUInt(height)}}
                Dim pixels = Await frame.GetPixelDataAsync(
                                BitmapPixelFormat.Rgba8,
                                BitmapAlphaMode.Premultiplied,
                                xform,
                                ExifOrientationMode.RespectExifOrientation,
                                ColorManagementMode.DoNotColorManage).Log("frame.GetPixelDataAsync")
                Dim src = pixels.DetachPixelData()
                '
                Dim scanlines = CInt(height)
                If NBRICKHEIGHT <> 0 Then
                    If scanlines > NHEIGHT * 2 \ 3 Then scanlines = NHEIGHT * 2 \ 3
                    scanlines = scanlines - (scanlines Mod NBRICKHEIGHT)
                End If
                Dim mbi As New MutableBitmapInfo()
                mbi._bitmap = New WriteableBitmap(width, scanlines)
                Using dst = mbi.LockPixels()
                    For i = 0 To width * scanlines * 4 - 1 Step 4
                        Const RED = 0, GREEN = 1, BLUE = 2, ALPHA = 3
                        dst.Stream.WriteByte(src(i + BLUE))
                        dst.Stream.WriteByte(src(i + GREEN))
                        dst.Stream.WriteByte(src(i + RED))
                        dst.Stream.WriteByte(src(i + ALPHA))
                    Next
                End Using
                Return mbi
            Catch ex As Exception
                Return Nothing
            End Try
        End Function

        Function Serialize() As BitmapSerializationProxy
            Dim p As New BitmapSerializationProxy With {.Width = Width, .Height = Height}
            Using bits = LockPixels(), mem As New MemoryStream()
                bits.Stream.CopyTo(mem)
                p.Content = mem.ToArray()
                Return p
            End Using
        End Function

        Shared Function Deserialize(p As BitmapSerializationProxy) As MutableBitmapInfo
            Dim mbi As New MutableBitmapInfo()
            mbi._bitmap = New WriteableBitmap(p.Width, p.Height)
            Using dst = mbi.LockPixels()
                dst.Stream.Write(p.Content, 0, p.Content.Length)
            End Using
            Return mbi
        End Function

        ReadOnly Property Bitmap As WriteableBitmap
            Get
                Return _bitmap
            End Get
        End Property
        Public ReadOnly Property Width As Integer
            Get
                Return _bitmap.PixelWidth
            End Get
        End Property
        Public ReadOnly Property Height As Integer
            Get
                Return _bitmap.PixelHeight
            End Get
        End Property
        Public Function LockPixels() As MutableBitmapLock
            Return New MutableBitmapLock(Bitmap)
        End Function

        Class MutableBitmapLock
            Implements IDisposable
            Private Bitmap As WriteableBitmap
            Public Stream As Stream

            Public Sub New(bmp As WriteableBitmap)
                Bitmap = bmp
                Stream = System.Runtime.InteropServices.WindowsRuntime.WindowsRuntimeBufferExtensions.AsStream(bmp.PixelBuffer)
            End Sub

            Public Sub Seek(x As Integer, y As Integer)
                Stream.Seek(y * Bitmap.PixelWidth * 4 + x * 4, SeekOrigin.Begin)
            End Sub

            Public Sub Write(c As Color)
                Stream.WriteByte(c.B)
                Stream.WriteByte(c.G)
                Stream.WriteByte(c.R)
                Stream.WriteByte(c.A)
            End Sub

            Public Property Pixel(x As Integer, y As Integer) As Color
                Get
                    Seek(x, y)
                    Dim b = Stream.ReadByte(), g = Stream.ReadByte(), r = Stream.ReadByte(), a = Stream.ReadByte()
                    Return Color.FromArgb(CByte(a), CByte(r), CByte(g), CByte(b))
                End Get
                Set(value As Color)
                    Seek(x, y)
                    Write(value)
                End Set
            End Property

            Public Sub Dispose() Implements IDisposable.Dispose
                Stream.Dispose()
                Bitmap.Invalidate()
            End Sub
        End Class

    End Class



    <Extension>
    Sub AddAt(this As Canvas, e As UIElement, x As Double, y As Double, zindex As Integer)
        Canvas.SetLeft(e, x)
        Canvas.SetTop(e, y)
        Canvas.SetZIndex(e, zindex)
        this.Children.Add(e)
    End Sub

    <Extension> Function Left(this As FrameworkElement) As Double
        Return Canvas.GetLeft(this)
    End Function
    <Extension> Function Top(this As FrameworkElement) As Double
        Return Canvas.GetTop(this)
    End Function
    <Extension> Function Right(this As FrameworkElement) As Double
        Return Canvas.GetLeft(this) + this.Width
    End Function
    <Extension> Function Bottom(this As FrameworkElement) As Double
        Return Canvas.GetTop(this) + this.Height
    End Function
    <Extension> Function GetBounds(this As FrameworkElement) As Rect
        Return New Rect(Canvas.GetLeft(this), Canvas.GetTop(this), this.Width, this.Height)
    End Function
    <Extension> Function Center(this As FrameworkElement) As Point
        Return New Point(Canvas.GetLeft(this) + this.Width / 2, Canvas.GetTop(this) + this.Height / 2)
    End Function
    <Extension> Function Center(this As Rect) As Point
        Return New Point(this.Left + this.Width / 2, this.Top + this.Height / 2)
    End Function


End Module
