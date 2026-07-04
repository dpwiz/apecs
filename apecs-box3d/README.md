# apecs-box3d

[apecs](https://hackage.haskell.org/package/apecs) integration for the
Box3D physics engine, via the
[`Box3D`](https://gitlab.com/dpwiz/box-nd) bindings — the 3D sibling of
apecs-box2d, with the same design: add `Physics` to your world, give
entities a `Body`, and work with the engine-backed sub-components
(`Position`, `Velocity`, `Rotation`, `Shape`, ...). Vectors are `Vec3`
and rotations are quaternions. The raw engine stays reachable through
`B3BodyId`, `B3ShapeId`, `B3JointId` and `getWorldId` — add `Box3D` to
your own build-depends to import the raw modules those ids unlock.

## Demo

`apecs-box3d-demo` is the tumbler one dimension up: a kinematic box
cage tumbling about two axes, full of spheres and cubes, rendered with
[apecs-gloss](https://hackage.haskell.org/package/apecs-gloss) through
a CPU perspective projection — painter's-algorithm depth sort across
bodies, backface culling within each convex cube, flat shading and
distance fog. Click to drop in more debris.

```sh
stack run apecs-box3d-demo
```
