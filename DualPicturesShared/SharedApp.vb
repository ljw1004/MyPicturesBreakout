Imports Windows.ApplicationModel.Store
Imports Windows.Storage
Imports Windows.Storage.Search

Public Class SharedApp

    ' Persisted data
    Private _HasGuidanceSystem As Boolean? = Nothing
    Private _IsTrial As Boolean? = Nothing
    Public HighScores As Scores
    Private _HighScoresFromOthers As Scores
    Private _LocalGuid As Guid
    ' Transient static data, initialized once
    Dim files As New List(Of Tuple(Of StorageFile, Integer, Integer)) ' this grows over time, updated by background thread, and is SyncLocked on itself
    Private wavePlayer As New WavePlayer
    Dim stockBackgroundsTask As Task(Of IReadOnlyList(Of StorageFile))
    Dim stockForegroundsTask As Task(Of IReadOnlyList(Of StorageFile))
    Dim PaddleBeep, BrickBeep, GuidedBeep, WinBeep, LoseBeep As StorageFile
    Dim RND As New System.Random
    'Public TelemetryClient As Microsoft.ApplicationInsights.TelemetryClient
    ' Dynamic data for the level currently being played
    Public Dat As GameData
    Public Event DatChanged()
    Public isStartingLevel As Boolean = False

    Public Shared ReadOnly Property Current As SharedApp
        Get
            Static Dim c As New SharedApp
            Return c
        End Get
    End Property

    Private Sub New()
    End Sub

    Public Sub App_OnLaunched()
        Try
            AddHandler ApplicationData.Current.DataChanged, AddressOf LoadRoamingScores
            If ApplicationData.Current.LocalSettings.Values.ContainsKey("LocalGuid") Then
                _LocalGuid = CType(ApplicationData.Current.LocalSettings.Values("LocalGuid"), Guid)
            Else
                _LocalGuid = Guid.NewGuid()
                ApplicationData.Current.LocalSettings.Values("LocalGuid") = _LocalGuid
            End If
            LoadRoamingScores(Nothing, Nothing)
            StartStaticInitialization()

            Dim rootFrame As Frame = CType(Window.Current.Content, Frame)
            If rootFrame Is Nothing Then
                rootFrame = New Frame()
                'If e.PreviousExecutionState = ApplicationExecutionState.Terminated Then ...
                StartLoadLocalGameState()
                Window.Current.Content = rootFrame
            End If
            'TODO!!!
            'If rootFrame.Content Is Nothing Then rootFrame.Navigate(GetType(MainPage), e.Arguments)
            Window.Current.Activate()
        Catch ex As Exception
            StartErrorReport("OnLaunched", ex)
        End Try
    End Sub

    Public Sub App_OnResuming()
        If wavePlayer Is Nothing Then wavePlayer = New WavePlayer
    End Sub

    Public Async Function App_OnSuspendingAsync() As Task
        Try
            RemoveHandler ApplicationData.Current.DataChanged, AddressOf LoadRoamingScores
            SaveRoamingScores()
            Await SaveLocalGameStateAsync().Log("SaveLocalGameStateAsync")
            wavePlayer.Dispose() : wavePlayer = Nothing
        Catch ex As Exception
            StartErrorReport("OnSuspendingAsync", ex)
        End Try
    End Function


    Async Sub StartStaticInitialization()
        Try
            Dim filesTask = Task.Run(AddressOf FileScannerAsync)
            stockBackgroundsTask = Task.Run(Async Function()
                                                Dim folder = Await Package.Current.InstalledLocation.GetFolderAsync("Assets\StockBackgrounds").Log("GetFolderAsync", "StockBackgrounds")
                                                Return Await folder.GetFilesAsync().Log("GetFilesAsync", "StockBackgrounds")
                                            End Function)
            stockForegroundsTask = Task.Run(Async Function()
                                                Dim folder = Await Package.Current.InstalledLocation.GetFolderAsync("Assets\StockForegrounds").Log("GetFolderAsync", "StockForegrounds")
                                                Return Await folder.GetFilesAsync().Log("GetFilesAsync", "StockForegrounds")
                                            End Function)
            Dim beepTask = Task.Run(Async Function()
                                        Dim folder = Await Package.Current.InstalledLocation.GetFolderAsync("Assets").Log("GetFolderAsync", "Assets")
                                        Dim paddleBeepTask = folder.GetFileAsync("beep-paddle.wav")
                                        Dim brickBeepTask = folder.GetFileAsync("beep-brick.wav")
                                        Dim guidedBeepTask = folder.GetFileAsync("beep-guided.wav")
                                        Dim winBeepTask = folder.GetFileAsync("win.wav")
                                        Dim loseBeepTask = folder.GetFileAsync("lose.wav")
                                        PaddleBeep = Await paddleBeepTask.Log("GetFileAsync", "beep-paddle.wav")
                                        BrickBeep = Await brickBeepTask.Log("GetFileAsync", "beep-brick.wav")
                                        GuidedBeep = Await guidedBeepTask.Log("GetFileAsync", "beep-guided.wav")
                                        WinBeep = Await winBeepTask.Log("GetFileAsync", "win.wav")
                                        LoseBeep = Await loseBeepTask.Log("GetFileASync", "lose.wav")
                                    End Function)
            Await Task.WhenAll(filesTask, stockBackgroundsTask, stockForegroundsTask, beepTask).Log("Task.WhenAll", "files/backgrounds/foregrounds/beepTask")
        Catch ex As Exception
            StartErrorReport("StartStaticInitialization", ex)
        End Try
    End Sub

    Async Function FileScannerAsync() As Task
        ' CommonFileQuery.OrderBySearchRank: works on Windows8.1, but Phone8.1 throws exception on all but DefaultQuery
        Dim queue As New Queue(Of StorageFolder)
        Dim folder = KnownFolders.PicturesLibrary

        While True ' outer loop over folders and subfolders
            Dim subfolders = Await folder.GetFoldersAsync().Log("GetFoldersAsync", folder.Path)
            For Each ff In subfolders : queue.Enqueue(ff) : Next
            Dim index = 0UI
            While True ' inner loop over files within folder
                Dim batch = Await folder.GetFilesAsync(CommonFileQuery.DefaultQuery, index, 10).Log("GetFilesAsync", index)
                If batch.Count = 0 Then Exit While
                index += CUInt(batch.Count)
                Dim prevcount = files.Count
                '
                Dim props = Await Task.WhenAll(batch.Select(Function(file) file.Properties.GetImagePropertiesAsync().AsTask())).Log("Task.WhenAll", "getImagePropertiesAsyncTasks")
                Dim toAdd As New List(Of Tuple(Of StorageFile, Integer, Integer))
                For i = 0 To batch.Count - 1
                    If batch(i).Path.Contains("Screenshots") Then Continue For
                    Dim w = CInt(props(i).Width), h = CInt(props(i).Height)
                    If w < 300 OrElse h < 300 Then Continue For
                    If w > 5000 OrElse h > 5000 Then Continue For
                    toAdd.Add(Tuple.Create(batch(i), w, h))
                Next
                '
                SyncLock files
                    files.AddRange(toAdd)
                End SyncLock
                '
                ' Every 100 files or so, pause for 10 seconds
                Dim newcount = files.Count
                If prevcount \ 100 < newcount \ 100 Then Await Task.Delay(10000).Log("Task.Delay", 10000)

            End While
            If queue.Count = 0 Then Exit While
            folder = queue.Dequeue()
        End While
    End Function

    Public Function HasGuidanceSystem() As Boolean
        If _HasGuidanceSystem IsNot Nothing Then Return _HasGuidanceSystem.Value
        Try
            _HasGuidanceSystem = CurrentApp.LicenseInformation.ProductLicenses("GuidanceSystem").IsActive
        Catch ex As Exception
            _HasGuidanceSystem = True ' for the BUILD preview, there's no store yet, so we fake it
        End Try
        Return _HasGuidanceSystem.Value
    End Function

    Public Async Function PurchaseGuidanceSystemAsync() As Task
        If HasGuidanceSystem() Then Return
        _HasGuidanceSystem = Nothing
        Dim log = CStr(ApplicationData.Current.LocalSettings.Values("PurchaseGuidanceSystemLog"))
        If log IsNot Nothing Then
            ApplicationData.Current.LocalSettings.Values.Remove("PurchaseGuidanceSystemLog")
            StartErrorReport("PurchaseGuidanceSystemAsync", log)
            _HasGuidanceSystem = True
            Return
        End If

        Try
            log = "About to call RequestProductPurchaseAsync(""GuidanceSystem"")"
            ApplicationData.Current.LocalSettings.Values("PurchaseGuidanceSystemLog") = log
            Dim task = CurrentApp.RequestProductPurchaseAsync("GuidanceSystem")
            log &= vbCrLf & "Got back IAsyncOperation with ex = " & If(task.ErrorCode Is Nothing, "null", task.ErrorCode.Message) & vbCrLf & "About to await it..."
            ApplicationData.Current.LocalSettings.Values("PurchaseGuidanceSystemLog") = log
            Dim result = Await task
            log &= vbCrLf & String.Format("Finished await. Status={0}, OfferID={1}, TransactionId={2}", result.Status, result.OfferId, result.TransactionId)
            ApplicationData.Current.LocalSettings.Values("PurchaseGuidanceSystemLog") = log
        Catch ex As Exception
            log &= vbCrLf & "EXCEPTION! ex.Message = " & ex.Message & vbCrLf & "Stack = " & ex.StackTrace & vbCrLf & "About to call StartErrorReport"
            ApplicationData.Current.LocalSettings.Values("PurchaseGuidanceSystemLog") = log
            StartErrorReport("PurchaseGuidanceSystemAsync", ex)
            log &= vbCrLf & "Finished Call To StartErrorReport"
            ApplicationData.Current.LocalSettings.Values("PurchaseGuidanceSystemLog") = log
        End Try
    End Function

    Public Function IsTrial() As Boolean
        If _IsTrial IsNot Nothing Then Return _IsTrial.Value
        Try
            _IsTrial = CurrentApp.LicenseInformation.IsActive AndAlso CurrentApp.LicenseInformation.IsTrial
        Catch ex As Exception
            _IsTrial = False ' for the BUILD preview, there's no store yet, so we fake it
        End Try
        Return _IsTrial.Value
    End Function

    Public Async Function PurchaseAppAsync() As Task
        If Not IsTrial() Then Return
        _IsTrial = Nothing

        Dim log = CStr(ApplicationData.Current.LocalSettings.Values("PurchaseAppLog"))
        If log IsNot Nothing Then
            ApplicationData.Current.LocalSettings.Values.Remove("PurchaseAppLog")
            StartErrorReport("PurchaseAppAsync", log)
            _IsTrial = False
            Return
        End If

        Try
            log = "About To Call RequestAppPurchaseAsync(False). IsActive=" & CurrentApp.LicenseInformation.IsActive & ", IsTrial=" & CurrentApp.LicenseInformation.IsTrial
            ApplicationData.Current.LocalSettings.Values("PurchaseAppLog") = log
            Dim task = CurrentApp.RequestAppPurchaseAsync(False)
            log &= vbCrLf & "Got back IAsyncOperation With ex = " & If(task.ErrorCode Is Nothing, "null", task.ErrorCode.Message) & vbCrLf & "About To await it..."
            ApplicationData.Current.LocalSettings.Values("PurchaseAppLog") = log
            Dim result = Await task
            log &= vbCrLf & "Finished await. Result = """ & result & """"
            ApplicationData.Current.LocalSettings.Values("PurchaseAppLog") = log
        Catch ex As Exception
            log &= vbCrLf & "EXCEPTION! ex.Message = " & ex.Message & vbCrLf & "Stack = " & ex.StackTrace & vbCrLf & "About To Call StartErrorReport"
            ApplicationData.Current.LocalSettings.Values("PurchaseAppLog") = log
            StartErrorReport("PurchaseAppAsync", ex)
            log &= vbCrLf & "Finished Call To StartErrorReport"
            ApplicationData.Current.LocalSettings.Values("PurchaseAppLog") = log
        End Try
    End Function

    Sub SaveRoamingScores()
        Dim localDelta = HighScores - _HighScoresFromOthers
        Dim localContainer = ApplicationData.Current.RoamingSettings.CreateContainer(_LocalGuid.ToString(), ApplicationDataCreateDisposition.Always)
        localDelta.Serialize(localContainer)
    End Sub

    Sub LoadRoamingScores(appData As ApplicationData, o As Object)
        Dim others As New Scores, local As New Scores
        For Each container In ApplicationData.Current.RoamingSettings.Containers
            If container.Key = _LocalGuid.ToString() Then
                local = Scores.Deserialize(container.Value)
            Else
                Dim other = Scores.Deserialize(container.Value)
                others = others + other
            End If
        Next
        If HighScores IsNot Nothing Then local = HighScores - _HighScoresFromOthers
        _HighScoresFromOthers = others
        HighScores = others + local
    End Sub


    Async Sub StartLoadLocalGameState()
        Try
            If isStartingLevel Then Throw New InvalidOperationException("Starting level")

            Dim loadFunc = Async Function(fn As String) As Task(Of Byte())
                               Try
                                   Dim file = Await ApplicationData.Current.LocalFolder.GetFileAsync(fn).Log("GetFileAsync", fn)
                                   Using s = Await file.OpenStreamForReadAsync().Log("file.OpenStreamForReadAsync", fn), s2 As New MemoryStream()
                                       s.CopyTo(s2)
                                       Return s2.ToArray()
                                   End Using
                               Catch ex As FileNotFoundException
                                   Return Nothing
                               End Try
                           End Function
            Try
                isStartingLevel = True
                RaiseEvent DatChanged()
                Dim task1 = Task.Run(Function() loadFunc("info.txt"))
                Dim task2 = Task.Run(Function() loadFunc("bricks.dat"))
                Dim task3 = Task.Run(Function() loadFunc("foreground.dat"))
                Dim task4 = Task.Run(Function() loadFunc("background.dat"))
                Await Task.WhenAll(task1, task2, task3, task4).Log("Task.WhenAll", "info/bricks/foreground/background_LoadFunc")
                Dim info = Await task1, b = Await task2, fg = Await task3, bg = Await task4
                If info Is Nothing OrElse b Is Nothing OrElse fg Is Nothing OrElse bg Is Nothing Then Return
                Try
                    Dat = GameData.Deserialize(info, b, fg, bg)
                Catch ex As Exception
                    Return ' BUG in D14Rel 22815: CoreCLR can't deserialize DataContract!
                End Try
            Finally
                isStartingLevel = False
                RaiseEvent DatChanged()
                If Dat Is Nothing Then StartNewLevel()
            End Try
        Catch ex As Exception
            StartErrorReport("StartLoadLocalGameState", ex)
        End Try
    End Sub

    Async Function SaveLocalGameStateAsync() As Task
        If Dat Is Nothing Then Return
        Dim info As Byte() = Nothing, b As Byte() = Nothing, fg As Byte() = Nothing, bk As Byte() = Nothing
        Dat.Serialize(info, b, fg, bk)

        Dim saveFunc = Async Function(fn As String, buf As Byte())
                           Dim file = Await ApplicationData.Current.LocalFolder.CreateFileAsync(fn, CreationCollisionOption.ReplaceExisting).Log("CreateFileAsync", fn & ":CollisionOption.ReplaceExisting")
                           Using s = Await file.OpenStreamForWriteAsync().Log("file.OpenStreamForWriteAsync", fn)
                               Await s.WriteAsync(buf, 0, buf.Length).Log("s.WriteAsync", buf.Length)
                           End Using
                       End Function
        Dim task1 = Task.Run(Function() saveFunc("info.txt", info))
        Dim task2 = Task.Run(Function() saveFunc("bricks.dat", b))
        Dim task3 = Task.Run(Function() saveFunc("foreground.dat", fg))
        Dim task4 = Task.Run(Function() saveFunc("background.dat", bk))
        Await Task.WhenAll(task1, task2, task3, task4).Log("Task.WhenAll", "info/bricks/foreground/background_saveFunc")
    End Function

    Async Sub StartNewLevel()
        Try
            If isStartingLevel Then Return
            isStartingLevel = True
            SaveRoamingScores()
            RaiseEvent DatChanged()
            '
            Do
                Dim background As StorageFile = Nothing, foreground As StorageFile = Nothing
                Dim portrait = True
                If files.Count > 0 Then
                    Dim ttfile = files(RND.Next(files.Count))
                    background = ttfile.Item1 : foreground = ttfile.Item1
                    portrait = ttfile.Item2 < ttfile.Item3
                End If
                If foreground Is Nothing OrElse portrait Then
                    Dim stockForegrounds = Await stockForegroundsTask.Log("stockForegroundsTask")
                    foreground = stockForegrounds(RND.Next(stockForegrounds.Count))
                End If
                If background Is Nothing OrElse Not portrait Then
                    Dim stockBackgrounds = Await stockBackgroundsTask.Log("stockBackgroundsTask")
                    background = stockBackgrounds(RND.Next(stockBackgrounds.Count))
                End If
                Dat = Await GameLogic.InitializeGameDataAsync(background, foreground).Log("GameLogic.InitializeGameDataAsync", background.Path & " " & foreground.Path)
            Loop Until Dat IsNot Nothing
        Catch ex As Exception
            StartErrorReport("StartNewLevel", ex)
        End Try
        isStartingLevel = False
        RaiseEvent DatChanged()
    End Sub

    Sub Tick(tinterval As TimeSpan)
        If Dat Is Nothing Then Return
        If isStartingLevel Then Return
        Dim wantBrickBeep = False, wantPaddleBeep = False, wantGuidedBeep = False
        Dim bricksDestroyed = 0UL, ballsLost = 0UL
        '
        GameLogic.UpdateBalls(Dat, tinterval, wantPaddleBeep, wantBrickBeep, wantGuidedBeep, ballsLost, bricksDestroyed)
        '
        If wantBrickBeep Then wavePlayer.StartPlay(BrickBeep)
        If wantPaddleBeep Then wavePlayer.StartPlay(PaddleBeep)
        If wantGuidedBeep Then wavePlayer.StartPlay(GuidedBeep)
        HighScores.BallsLost += ballsLost
        HighScores.BricksDestroyed += bricksDestroyed

        Dim wantNewLevel = False
        If Dat.BrickCount = 0 Then
            wavePlayer.StartPlay(WinBeep) : wantNewLevel = True : HighScores.GamesWon += 1UL
        ElseIf Dat.Balls.Count = 0 Then
            wavePlayer.StartPlay(LoseBeep) : wantNewLevel = True : HighScores.GamesLost += 1UL
        End If
        If wantNewLevel Then StartNewLevel()
    End Sub

    Sub StartErrorReport(location As String, ex As Exception)
        Dim msg = ex.Message & vbCrLf & vbCrLf & "Stack: " & vbCrLf & ex.StackTraceEx & vbCrLf & "OriginalStack: " & vbCrLf & ex.StackTrace.ToString()
        StartErrorReport(location, msg)
    End Sub

    Async Sub StartErrorReport(location As String, msg As String)
        Stop
        Dim md As New Windows.UI.Popups.MessageDialog("Oops. There's been an internal error.", "Bug report")
        Dim r As Boolean? = Nothing
        md.Commands.Add(New Windows.UI.Popups.UICommand("Send bug report", Sub() r = True))
        md.Commands.Add(New Windows.UI.Popups.UICommand("Cancel", Sub() r = False))
        Await md.ShowAsync().Log("MessageDialog.ShowAsync")
        If Not r.HasValue OrElse Not r.Value Then Return
        '
        Dim emailTo = "lu@wischik.com"
        Dim emailSubject = "MyPicturesBreakout problem report"
        Dim emailBody = "I encountered a problem with Breakout Universal..." & vbCrLf & vbCrLf & location & " error:" & vbCrLf & msg
        Dim url = "mailto:?to=" & emailTo & "&subject=" & emailSubject & "&body=" & Uri.EscapeDataString(emailBody)
        Await Windows.System.Launcher.LaunchUriAsync(New Uri(url))
    End Sub

End Class


Public Class Scores
    Public GamesWon As ULong
    Public GamesLost As ULong
    Public BallsSaved As ULong
    Public BallsLost As ULong
    Public BricksDestroyed As ULong
    Public TimePlayed As TimeSpan

    Public Sub New()
    End Sub

    Public Sub New(s As Scores)
        GamesWon = s.GamesWon
        GamesLost = s.GamesLost
        BallsSaved = s.BallsSaved
        BallsLost = s.BallsLost
        BricksDestroyed = s.BricksDestroyed
        TimePlayed = s.TimePlayed
    End Sub

    Public Sub Serialize(d As ApplicationDataContainer)
        d.Values("GamesWon") = GamesWon
        d.Values("GamesLost") = GamesLost
        d.Values("BallsSaved") = BallsSaved
        d.Values("BallsLost") = BallsLost
        d.Values("BricksDestroyed") = BricksDestroyed
        d.Values("TimePlayed") = TimePlayed
    End Sub

    Public Shared Function Deserialize(d As ApplicationDataContainer) As Scores
        Dim s As New Scores
        s.GamesWon = CULng(d.Values("GamesWon"))
        s.GamesLost = CULng(d.Values("GamesLost"))
        s.BallsSaved = CULng(d.Values("BallsSaved"))
        s.BallsLost = CULng(d.Values("BallsLost"))
        s.BricksDestroyed = CULng(d.Values("BricksDestroyed"))
        s.TimePlayed = CType(d.Values("TimePlayed"), TimeSpan)
        Return s
    End Function

    Public Shared Operator +(a As Scores, b As Scores) As Scores
        Dim c As New Scores
        c.GamesWon = a.GamesWon + b.GamesWon
        c.GamesLost = a.GamesLost + b.GamesLost
        c.BallsSaved = a.BallsSaved + b.BallsSaved
        c.BallsLost = a.BallsLost + b.BallsLost
        c.BricksDestroyed = a.BricksDestroyed + b.BricksDestroyed
        c.TimePlayed = a.TimePlayed + b.TimePlayed
        Return c
    End Operator

    Public Shared Operator -(a As Scores, b As Scores) As Scores
        Dim c As New Scores
        c.GamesWon = a.GamesWon - b.GamesWon
        c.GamesLost = a.GamesLost - b.GamesLost
        c.BallsSaved = a.BallsSaved - b.BallsSaved
        c.BallsLost = a.BallsLost - b.BallsLost
        c.BricksDestroyed = a.BricksDestroyed - b.BricksDestroyed
        c.TimePlayed = a.TimePlayed - b.TimePlayed
        Return c
    End Operator
End Class
