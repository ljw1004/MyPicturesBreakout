﻿Imports Windows.ApplicationModel.Store
Imports Windows.Devices.Input
Imports Windows.UI.Xaml.Navigation

Public NotInheritable Class MainPage
    Inherits Page

    Dim Pointers As New Dictionary(Of UInteger, Point)
    Dim CurrentReleaseStarted As DateTime?
    Dim CurrentPlayStarted As DateTime?
    Dim TickWatch As New Stopwatch
    Dim isPageOpen As Boolean = False

    Protected Overrides Async Sub OnNavigatedTo(e As NavigationEventArgs)
        Try
            AddHandler SharedApp.Current.DatChanged, AddressOf OnDatChanged
            isPageOpen = True
            UpdateDashboard()
            TickWatch.Start()
            While isPageOpen
                Try
                    Dim t = Task.Delay(15)
                    If CurrentPlayStarted IsNot Nothing Then SharedApp.Current.Tick(TickWatch.Elapsed) : TickWatch.Reset() : TickWatch.Start()
                    If CurrentPlayStarted IsNot Nothing AndAlso CurrentReleaseStarted IsNot Nothing AndAlso (DateTime.Now - CurrentReleaseStarted.Value).TotalMilliseconds > 50 Then StopPlay()
                    Await t.Log("Task.Delay", 15)
                Catch ex As Exception
                    SharedApp.Current.StartErrorReport("Loop", ex)
                End Try
            End While
        Catch ex As Exception
            SharedApp.Current.StartErrorReport("OnNavigatedTo", ex)
        End Try
    End Sub

    Protected Overrides Sub OnNavigatedFrom(e As NavigationEventArgs)
        Try
            isPageOpen = False
            RemoveHandler SharedApp.Current.DatChanged, AddressOf OnDatChanged
        Catch ex As Exception
            SharedApp.Current.StartErrorReport("OnNavigatedFrom", ex)
        End Try
    End Sub

    Sub OnDatChanged()
        Try
            playArea1.Content = Nothing
            If SharedApp.Current.Dat IsNot Nothing Then playArea1.Content = SharedApp.Current.Dat.Canvas
            OnResize(Nothing, Nothing)
            UpdateDashboard()
        Catch ex As Exception
            SharedApp.Current.StartErrorReport("OnDatChanged", ex)
        End Try
    End Sub

    Sub OnResize(sender As Object, e As SizeChangedEventArgs) Handles Me.SizeChanged
        Try
            Dim w = CInt(ActualWidth), h = CInt(ActualHeight)
            If h > w * 5 \ 3 Then h = w * 5 \ 3 Else w = h * 3 \ 5
            If SharedApp.Current.Dat IsNot Nothing Then GameLogic.Resize(SharedApp.Current.Dat, w, h)
            grid1.Width = w : grid1.Height = h
            paused2.Width = w
            '
            Dim percent = 100 * h / ActualHeight * w / ActualWidth
            image1.Visibility = If(percent < 50 OrElse SharedApp.Current.Dat Is Nothing, Visibility.Visible, Visibility.Collapsed)
        Catch ex As Exception
            SharedApp.Current.StartErrorReport("OnResize", ex)
        End Try
    End Sub

    Sub StartPlay()
        If CurrentPlayStarted Is Nothing Then CurrentPlayStarted = DateTime.Now
        CurrentReleaseStarted = Nothing
        UpdateDashboard()
        TickWatch.Start()
    End Sub

    Sub StopPlay()
        TickWatch.Stop()
        If CurrentPlayStarted IsNot Nothing Then SharedApp.Current.HighScores.TimePlayed += DateTime.Now - CurrentPlayStarted.Value
        CurrentPlayStarted = Nothing
        UpdateDashboard()
    End Sub

    Protected Overrides Sub OnPointerPressed(e As PointerRoutedEventArgs)
        Try
            e.Handled = True
            CurrentReleaseStarted = Nothing
            Pointers(e.Pointer.PointerId) = PointerToPoint(e)
            If CurrentPlayStarted Is Nothing Then StartPlay()
        Catch ex As Exception
            SharedApp.Current.StartErrorReport("OnPointerPressed", ex)
        End Try
    End Sub

    Function PointerToPoint(e As PointerRoutedEventArgs) As Point
        Dim pt = e.GetCurrentPoint(grid1).Position
        If e.Pointer.PointerDeviceType = PointerDeviceType.Touch Then
            Dim finger = 40.0
            Dim info = DisplayInformation.GetForCurrentView()
            If info.RawDpiX > 0 Then finger = 0.3 * info.RawDpiX / info.ResolutionScale * 100
            pt.Y -= finger
        End If
        Return pt
    End Function

    Protected Overrides Sub OnPointerMoved(e As PointerRoutedEventArgs)
        Try
            e.Handled = True
            If Pointers.ContainsKey(e.Pointer.PointerId) Then Pointers(e.Pointer.PointerId) = PointerToPoint(e)
            If SharedApp.Current.Dat IsNot Nothing Then GameLogic.HandlePointers(SharedApp.Current.Dat, Pointers)
        Catch ex As Exception
            SharedApp.Current.StartErrorReport("OnPointerMoved", ex)
        End Try
    End Sub

    Protected Overrides Sub OnPointerReleased(e As PointerRoutedEventArgs)
        Try
            e.Handled = True
            If Pointers.ContainsKey(e.Pointer.PointerId) Then Pointers.Remove(e.Pointer.PointerId)
            If Pointers.Count = 0 Then CurrentReleaseStarted = DateTime.Now
            If SharedApp.Current.Dat IsNot Nothing Then GameLogic.HandlePointers(SharedApp.Current.Dat, Pointers)
        Catch ex As Exception
            SharedApp.Current.StartErrorReport("OnPointerReleased", ex)
        End Try
    End Sub

    Sub UpdateDashboard()
        buyGuidanceSystem.Visibility = If(SharedApp.Current.HasGuidanceSystem, Visibility.Collapsed, Visibility.Visible)
        buyApp.Visibility = If(SharedApp.Current.IsTrial(), Visibility.Visible, Visibility.Collapsed)
        '
        Dim bricks = If(SharedApp.Current.HighScores.BricksDestroyed < 10000, CStr(SharedApp.Current.HighScores.BricksDestroyed), SharedApp.Current.HighScores.BricksDestroyed.ToString("#,##0").Replace(",", " "))
        scoreBricks.Text = bricks & " bricks"
        scoreGames.Text = CStr(SharedApp.Current.HighScores.GamesWon) & " games"
        Dim hours = SharedApp.Current.HighScores.TimePlayed.Days * 24 + SharedApp.Current.HighScores.TimePlayed.Hours
        Dim time = If(hours > 0, hours & "h", "")
        If hours < 10 Then time &= SharedApp.Current.HighScores.TimePlayed.Minutes & "m"
        If hours = 0 Then time &= SharedApp.Current.HighScores.TimePlayed.Seconds & "s"
        scoreTime.Text = time & " played"
        '
        paused1.Visibility = If(CurrentPlayStarted Is Nothing, Visibility.Visible, Visibility.Collapsed)
        paused2.Visibility = paused1.Visibility
        loading1.Visibility = If(SharedApp.Current.Dat Is Nothing AndAlso SharedApp.Current.isStartingLevel, Visibility.Visible, Visibility.Collapsed)
    End Sub

    Private Async Sub buyGuidanceSystem_Click(sender As Object, e As RoutedEventArgs) Handles buyGuidanceSystem.Click
        Try
            Await SharedApp.Current.PurchaseGuidanceSystemAsync().Log("PurchaseGuidanceSystemAsync")
            UpdateDashboard()
        Catch ex As Exception
            SharedApp.Current.StartErrorReport("buyGuidanceSystem_Click", ex)
        End Try
    End Sub

    Private Async Sub buyApp_Click(sender As Object, e As RoutedEventArgs) Handles buyApp.Click
        Try
            Await SharedApp.Current.PurchaseAppAsync().Log("PurchaseAppAsync")
            UpdateDashboard()
        Catch ex As Exception
            SharedApp.Current.StartErrorReport("buyApp_Click", ex)
        End Try
    End Sub

    Private Sub paused1_PointerMoved(sender As Object, e As PointerRoutedEventArgs) Handles paused1.PointerMoved
        e.Handled = True
    End Sub
End Class