# MyPicturesBreakout

This is a Breakout clone that uses your own pictures. You can control your brick with touch (and multi-touch!)

As an experiment, I made two versions of the codebase to see how they compared:

* **Win10-only**. If I think that Win10 is the future, and everyone will upgrade to it soon, then there's no point supporting older platforms. So this version is a pure Win10 "Universal Windows App" that can run on any Win10 device.
   * `MyPicturesBreakout10`

* **Win10 and WP8.1**. If I think that Windows Phone 8.1 will still be around for a while (because folks can't upgrade their phones easily) then I'll want to support it as well as Win10. So this version supports both.
   * `DualPictures10` - this project is a Win10 universal windows app.
   * `DualPictures81` - this project is specifically for Windows Phone 8.1
   * `DualPicturesShared` - this is a shared PCL that can be used by both. To make a PCL that can be used by both UAP and 8.1, you set its PCL targets to be *both* Win8.1 and WP8.1. With these two targets, it will implicitly work on UAP as well.
