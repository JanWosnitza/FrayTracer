# FrayTracer
A F# ray tracer.

Are you ready for the fray tracer?

# Ideas/Goals
* A Ray has an intensity and a wave-length (eg. optical dispersion, red-shift)
* Immutable scenes (eg. composable, thread-safe)
* Extensible
  * Materials (eg. wood, glass, cloud)
    * May create subsequent rays
    * Determines intensity of "trigger" ray
  * Object-Volumes (eg. sphere, cone, chair)
    * Ray collision passes control to a material (a ray hits one object only)

# TODO
* remove bad jokes
