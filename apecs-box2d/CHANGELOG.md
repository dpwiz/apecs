# Changelog

## [0.1.0.0]
### Added
- `Apecs.Box2D`, modelled on apecs-physics: the `Physics` world
  component with `Gravity`, `Substeps` and `destroyPhysics`, the
  `Body` component with
  `Position`/`Velocity`/`Angle`/`AngularVelocity`/`BodyMass`/`Force`/
  `Torque` sub-components, the `Shape` component (circle, capsule,
  segment, box, convex polygon) with `Density`/`Friction`/`Elasticity`,
  and the
  `B2BodyId`/`B2ShapeId`/`getWorldId` escape hatches to the raw engine.
- The `Joint` component (pivot, distance, weld, spring, slide, rotary
  spring/limit, motor; world-space specs) with the `B2JointId` escape
  hatch.
- `apecs-box2d-gallery`: a joint gallery after the apecs-physics
  Constraints example.
- Body dynamics extras: `LinearImpulse`/`AngularImpulse` appliers and
  `LinearDamping`/`AngularDamping`/`GravityScale`.
- `CollisionFilter` on shapes, re-exporting `Filter`.
- `apecs-box2d-demo`: a tumbler scene driven by Box2D and rendered with
  apecs-gloss.
