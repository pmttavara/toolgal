# ToolGal Tool Manager

This is the C++ source to a tool management program I wrote for George Brown. It's a Win64 checkin/checkout application with some general database operations built in. The code was intentionally architected in a data-oriented fashion, because the program this was tasked to replace used SQL databases and object-oriented code, which reduced performance.

To ensure that performance was totally controllable, no garbage collector or managed runtime was used for the GUI or other subsystems. UI, input, and rendering are completely custom, built on the SDL2 platform layer.

Of potential engineering interest in this project is a simple stack-based linear "temporary" allocator which allows for O(1) effective automatic memory management. It's not identical to a generational garbage collector, because it doesn't track object lifetimes, but the code is written with the knowledge that the lifetime of an object allocated by the temporary allocator will end at the frame boundary. (The GUI is what some call an "immediate mode GUI", and it renders at a consistent interval in order to draw smooth animations.)

The source contains a few bits of hacky code, but I've presented it as-is.

### Building

Visual Studio 2017 is required.

To build:

```
C:\toolgal>project build
```

To build in release mode:

```
C:\toolgal>project build release
```

Take note that the output executable name will be that of the surrounding folder, and will be located in `bin\`.

### License

Copyright 2017-2019 Phillip Trudeau-Tavara. All rights reserved.

