# FrayTracer
A F# ray tracer.

Are you ready for the fray tracer?

# Ideas/Goals
* A ray-test consists of a start-position, a direction and a wave-length and produces an intensity (eg. optical dispersion, red-shift)
* Immutable scenes (eg. composable, thread-safe)
* Extensible
  * Materials (eg. wood, glass, cloud)
    * May create subsequent rays
    * Determines intensity of "trigger" ray
  * Object-Volumes (eg. sphere, cone, chair)
    * Ray collision passes control to a material (a ray hits one object only)

# TODO
* remove bad jokes
