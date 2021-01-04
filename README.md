# ProxILGen

Generate dynamic pre-compiled proxies for mapping between objects. The source object will be embedded and all of the choosen properties/methods with the same type will be forwarded to it. 

We don't provide a customization endpoint, just directly filter the property list.
This project is similar in spirit to https://github.com/MetSystem/EmitMapper, but simpler and direct on the point, just emit the proxy, you use F# to define how the proxy structure should be.


## Installing

You should use Paket's github dependency to include the ProxILGen.fs file into your
project.

  https://fsprojects.github.io/Paket/github-dependencies.html

Or in short `packet.dependencies`:

  github Luiz-Monad/ProxILGen ProxILGen.fs


Also, if you want to compile out-of-the box you can just add Luiz-Monad/ProxILGen.fs to your SCM system.
