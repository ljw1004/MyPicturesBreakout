Imports DualPicturesShared

NotInheritable Class App
    Inherits Application

    Protected Overrides Sub OnLaunched(e As LaunchActivatedEventArgs)
        SharedApp.Current.App_OnLaunched()
    End Sub

    Private Sub OnResuming() Handles Me.Resuming
        SharedApp.Current.App_OnResuming()
    End Sub

    Private Async Sub OnSuspending(sender As Object, e As SuspendingEventArgs) Handles Me.Suspending
        Dim deferral As SuspendingDeferral = e.SuspendingOperation.GetDeferral()
        Await SharedApp.Current.App_OnSuspendingAsync()
        deferral.Complete()
    End Sub

End Class
