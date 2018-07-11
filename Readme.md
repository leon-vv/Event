Event
============================

An Idris package to handle the asynchronous nature of JavaScript. This package provides an `Event` type of two different kinds: Single and Multiple. The `Event Single` type represents events that are known to only call their callback once. This type of Event is an instance of the Monad typeclass. The `Event Multiple` represents events whose callback might be called any number of times (for example when listening to HTTP requests). This type of Event is only an instance of the Functor typeclass. 

Usage
-----------------------------
Make sure to install the latest version of the Idris compiler. This package has a dependency on the Record and FerryJS packages. So please install these first. Then run:
```idris --install event.ipkg```
To use the library in another file use:
```idris -p record_ -p ferryjs -p event Main.idr```

Documentation
----------------------------
```idris --mkdoc ./event.ipkg```

License
----------------------------
Mozilla Public License, v. 2.0
