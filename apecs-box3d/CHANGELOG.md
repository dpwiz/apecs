# Changelog

## [0.1.0.0]
### Added
- `Apecs.Box3D`, mirroring apecs-box2d in 3D: the `Physics` world
  component with `Gravity`, `Substeps` and `destroyPhysics`, the `Body`
  component with `Position`/`Velocity`/`Rotation`/`AngularVelocity`/
  `BodyMass`/`Force`/`Torque` sub-components, the `Shape` component
  (sphere, capsule, box, convex hull) with
  `Density`/`Friction`/`Elasticity`, and the
  `B3BodyId`/`B3ShapeId`/`getWorldId` escape hatches to the raw engine.
- The `Joint` component (spherical pivot, distance, weld; world-space
  specs) with the `B3JointId` escape hatch.
- Body dynamics extras: `LinearImpulse`/`AngularImpulse` appliers and
  `LinearDamping`/`AngularDamping`/`GravityScale`.
- `CollisionFilter` on shapes, re-exporting `Filter`.
- `apecs-box3d-demo`: a 3D tumbler rendered with apecs-gloss via a CPU
  perspective projection and painter's-algorithm depth sorting.
