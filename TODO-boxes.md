# TODO: apecs-box2d / apecs-box3d coverage vs upstream box-nd

Comparison of `Apecs.Box2D` / `Apecs.Box3D` against the upstream bindings at
`../box-nd/` (packages `Box2D`, `Box3D`), 2026-07-05. Both wrappers share one
design, so most items apply to both; per-package notes are marked **[2D]** /
**[3D]**.

## What is already covered

- **World**: gravity, substeps, step, destroy, `getWorldId` escape hatch.
- **Body**: type, position, rotation, linear/angular velocity, mass (read),
  center force/torque/impulses, damping, gravity scale, bullet flag.
- **Shape**: [2D] circle/capsule/segment/box/polygon, [3D] sphere/capsule/box/hull;
  density, friction, restitution, collision filter.
- **Joint**: [2D] pivot/distance/weld/spring/slide/rotary-spring/rotary-limit/motorised-pivot,
  [3D] pivot/distance/weld only. Recreate-on-set semantics.
- **Queries**: closest ray, broad-phase AABB/point query.
- **Events**: contact begin-touch (`Collisions`), hit (`Impacts`).
- Raw ids (`B2/B3 BodyId/ShapeId/JointId`) make everything else *reachable*,
  and no demo currently needs them — the core loop is sound.

## Impact × effort matrix

|                 | **Low effort**                                                                | **Medium effort**                                                       | **High effort**                        |
| --------------- | ----------------------------------------------------------------------------- | ----------------------------------------------------------------------- | -------------------------------------- |
| **High impact** | Sensors, end-touch events, point force/impulse, sleep/wake, motion locks, kinematic targets | [3D] hinge & slider joints, exact-geometry queries, joint tuning components | [3D] mesh/heightfield/compound shapes, character mover |
| **Med impact**  | explode, world tuning knobs, enabled flag, body-move events                    | [2D] wheel/prismatic joints, breakable joints, manifold data on Collisions | recording/replay, WorldDef config + task system |
| **Low impact**  | rounded boxes [2D], body name, CoM/inertia reads                               | chain shapes [2D], pre-solve callback, per-body shape lists              | debug draw, dynamic-tree API, allocator hooks |

Rule applied: top-left first; bottom-right is explicitly *not worth wrapping*
(escape hatch already serves it).

---

## P1 — Quick wins (high impact, small effort)

- [x] **`Sensor` component + `SensorEvents` global.** No trigger-volume story at
  all today; apecs-physics has `Sensor`, so ports hit this wall first.
  Upstream: `shapeDefIsSensor`, `Shape.enableSensorEvents`,
  `Events.sensorBegin/EndTouchEvents`. Mirror the `Collisions` pattern.
  Note: sensor events default *off*; opt in like contact/hit events in `explInit`. (S)
- [x] **`CollisionsEnd` global (separation events).** Begin-touch is exposed,
  end-touch is not, so "standing on ground" bookkeeping can't be done
  symmetrically. Upstream: `Events.contactEndTouchEvents` — already bound,
  reader is a copy of the begin-touch one. (S)
- [x] **Point-application force/impulse: `ForceAt`, `ImpulseAt` write-only
  components.** Only center application exists, so nothing can induce spin from
  an off-center hit — a big feel gap for combat/physics games. Upstream:
  `Body.applyForce`, `Body.applyLinearImpulse` (world point variants). (S)
- [x] **Sleep control: `Awake`, `SleepEnabled`, `SleepThreshold` body
  components + world `SleepingEnabled`.** Needed both for perf on large scenes
  and for gameplay (explicitly waking bodies after teleport-by-`Position`).
  Upstream: `Body.isAwake/setAwake/enableSleep/setSleepThreshold`,
  `World.enableSleeping`. (S)
- [x] **`MotionLocks` component (a.k.a. fixed rotation).** Top-down and
  platformer characters need locked rotation on day one. Upstream:
  `Body.setMotionLocks/getMotionLocks`. [2D] expose as `FixedRotation Bool`
  sugar too. (S)
- [x] **Kinematic target transform: `TargetTransform`/`TargetPosition`
  write-only component.** Moving platforms via `setTransform` teleport and
  don't impart velocity; `setTargetTransform` is the correct engine path. (S)
- [x] **`BodyEnabled` component.** Cheap despawn/pooling without destroying
  bodies. Upstream: `Body.enable/disable/isEnabled`. (S)
- [x] **`explode` system function.** One world call (`World.explode`,
  `ExplosionDef` exists with defaults); disproportionate fun-per-line. (S)
- [x] **World tuning components: `HitEventThreshold`, `RestitutionThreshold`,
  `MaximumLinearSpeed`, `ContinuousEnabled`.** Each is a trivial global
  get/set pair; `Impacts` docs already tell users to reach for the escape
  hatch for the hit threshold — make it a component instead. (S)

## P2 — Big bets (high impact, real effort)

- [x] **[3D] Fill out `JointSpec`: hinge (revolute), slider (prismatic),
  wheel.** 3D has only pivot/distance/weld — no doors, no vehicles, no
  pistons. Upstream modules `RevoluteJoint`, `PrismaticJoint`, `WheelJoint`
  are fully bound with defaults; follows the existing `createJoint` pattern.
  Include the 2D-style sugar variants (limits/springs/motors) where the def
  supports them. (M)
- [x] **[2D] Add `PrismaticJoint`/`WheelJoint` (and Box2D's actual
  `MotorJoint`) to `JointSpec`.** Wheel joint is the vehicle staple; the
  current `MotorJoint` name is a motorised revolute — consider renaming to
  `RotaryMotorJoint` when the real motor joint lands to avoid a trap. (M)
- [x] **Exact-geometry queries.** Today `aabbQuery`/`pointQuery` are
  broad-phase only — a *semantic trap* for apecs-physics users whose
  `pointQuery` was exact. Add: exact point test (`Shape.testPoint` [2D] /
  `Body.getClosestPoint`), `overlapShape` world query, all-hits
  `castRay` (list of `RayHit`), and `castShape` sweeps. Callback plumbing
  (`withCastResultFcn`, `withOverlapResultFcn`) is already available. (M)
  *Done as `containsPointQuery` + `segmentQueryAll`; 2026-07-06, with
  box-nd 33ecce4's Storable 2D `ShapeProxy`, also `overlapQuery` +
  `sweepQuery` [2D]. The 3D twins wait on a 3D `withShapeProxy`
  (NOTES-upstream §A).*
- [x] **Joint tuning sub-components on the joint entity: `MotorSpeed`,
  `MotorMaxTorque`/`MaxForce`, `JointLimits`, `CollideConnected`,
  read-only `JointForce`/`JointTorque`.** Re-setting `Joint` recreates the
  engine joint (resets accumulated state), so live motor control — the whole
  point of motors — currently requires the raw escape hatch. Needs a small
  per-type dispatch on the stored `JointSpec`. `CollideConnected` also restores
  apecs-physics `CollideBodies` parity. (M)
- [x] **[3D] Static world geometry: `GeoMesh`, `GeoHeightField`, `GeoCompound`.**
  A 3D physics binding without triangle meshes or heightfields cannot load a
  level; this is the biggest absolute gap. Upstream: `Shape.createMesh/
  createHeightField/createCompound` plus data lifecycles (`Mesh.create/destroy`,
  `HeightField.create/destroy`, `Compound.create/destroy`) and the procedural
  generators (`BoxMesh`, `GridMesh`, `TorusMesh`, `WaveMesh`, `Rock`, `Cone`,
  `Cylinder`, `Grid`, `Wave`, ...). Box3D owns
  `MeshData` (shared between shapes?) — suggest a `ShapeRecord`-style
  refcounted registry, generators exposed as plain `IO Geometry` helpers. (L)
  *Done as `GeoMesh`/`GeoHeightField`/`GeoReadyHull` over GC-managed
  `Mesh`/`HeightField`/`Hull` handles (the `ShapeRecord` registry keeps the
  ForeignPtr alive while any shape uses it) plus all procedural generators.
  `GeoCompound` deferred: `CompoundDef` is an opaque tag in box-nd with no
  Haskell constructor, same upstream gap as `ShapeProxy`.*
- [x] **Character mover support: `castMover`/`collideMover`/`solvePlanes`.**
  The engine-blessed kinematic character controller; platformers and
  first-person demos need it and hand-rolling it via rays is much worse.
  Could be a `moveCharacter` system function rather than components. (M/L)

## P3 — Fill-ins and infrastructure (do opportunistically)

- [x] **Breakable joints: `JointForceThreshold`/`JointTorqueThreshold`
  components + `JointEvents` global.** Upstream: `Joint.setForceThreshold/
  setTorqueThreshold`, `Events.jointEvents`. Pairs naturally with the P2
  joint-tuning work. (S once P2 joint work exists)
- [x] **Manifold/contact data on `Collisions`.** Begin-touch events carry
  contact data upstream; exposing point/normal would save a follow-up query.
  Check what `ContactBeginTouchEvent` actually carries before promising. (M)
  *Checked 2026-07-05: the event carries only shape ids + a `ContactId`,
  and `Contact.getData` writes into a `Ptr ContactData` that is an opaque
  tag in box-nd (no Storable, no allocator) — blocked on upstream exposing
  `ContactData`/`Manifold` as peekable structs.*
- [x] **Body-move events global (`Moved`).** Efficient render-sync (only
  bodies that actually moved, with sleep flag) instead of iterating every
  `Position`. Upstream: `Events.bodyMoveEvents`. (S)
- [x] **Recording / replay / snapshots.** `World.startRecording/stopRecording`,
  `Collision.saveRecordingToFile/loadRecordingFromFile/validateReplay`,
  `RecPlayer`, and [2D-only] `World.snapshot/restore/createFromSnapshot`.
  High value for *this repo's* demo-verification workflow (deterministic
  replay beats screenshot probing), but a niche of one — wrap as plain
  functions, no components. Note the 2D/3D asymmetry (no 3D snapshot). (M)
- [x] **World init configuration.** `explInit` bakes in `defaultWorldDef`, so
  worker count / task system (`withThreadPoolTaskSystem`, WorldDef enqueue
  callbacks), capacities and bounds are unreachable at creation. Options:
  read config from a `Global` the user sets before init, or provide
  `initPhysicsWith :: WorldDef -> ...`. `World.setWorkerCount` post-create
  may cover the common case cheaply — verify. (M)
  *Common case done as the `WorkerCount` global (settable any time between
  steps). Full WorldDef-at-creation config (task system, capacities,
  bounds) still open — needs an `initPhysicsWith` store-bootstrap path,
  which would also unlock wrapping 2D `createFromSnapshot`.*
- [x] **[2D] Chain shapes (`GeoChain`).** Terrain outlines without ghost
  collisions. Separate `ChainId` lifecycle, so it doesn't fit `ShapeRecord`
  directly. (M)
- [x] **[2D] Rounded/offset geometry constructors.** `makeRoundedBox`,
  `makeOffsetBox`, `makeOffsetRoundedPolygon` — cheap `Geometry` additions. (S)
- [ ] **Pre-solve callback (one-way platforms).** apecs-physics exposes
  pre-solve; here it needs FunPtr lifetime management tied to the space.
  Only worth it with a concrete demo driving it. (M)
- [x] **Small parity reads:** `CenterOfMass` (read), `RotationalInertia`
  (read, apecs-physics `Moment`), `BodyName`, per-body `ShapeList`/`JointList`
  (derivable from the registries, no FFI needed). (S each)

## Review follow-ups (2026-07-06)

Deferred findings from the whole-branch code review. The review's direct
fixes are already in (kind-filtered joint-tuning members, sensor flag
carried across `Shape` recreate, 3D destroy-before-unregister ordering,
3D sphere/capsule containment, `containsPointQuery` test-before-resolve,
doc warnings on `Chain` and `Sensor`). Triaged 2026-07-06 into the next
apecs batch vs. items blocked on (or better fixed in) box-nd first — the
upstream side is spelled out in `NOTES-upstream.md`.

### Next batch (apecs-side, nothing upstream in the way)

- [ ] **Joint tuning does not survive `Joint` re-set.** `MotorSpeed`,
  `MotorMaxTorque`, `MotorMaxForce` and `JointLimits` mutate the live
  engine joint only, so re-setting `Joint` (recreate-on-set) silently
  resets motors and limits — the joint-side analogue of the sensor-flag
  bug fixed in `carryMaterial`. Either read the tuning back from the old
  joint per kind and carry it over the recreate, or store it in
  `JointRecord`. See cross-cutting note 2. (M)
- [ ] **Derive joint kind without FFI.** `jointIsKind` and
  `jointKindMembers` do a `Joint.getType` round-trip per joint, and every
  tuning component get/set does another to dispatch — but the stored
  `JointSpec` constructor already determines the engine type. A pure
  spec → kind mapping (or a `JointType` cached in `JointRecord` at
  creation) makes exists/members/dispatch FFI-free. (S)
- [x] **Chain segment resolution.** Chain segments stay invisible to
  `Collisions`/`Impacts` *and* the queries; `segmentQuery` returns
  `Nothing` outright when a chain segment is the closest hit — the chain
  occludes whatever lies behind it. Documented on `Chain` now, but the
  real fix is a resolution mechanism: stamp the segment `ShapeId`s
  (`Chain.getSegmentCount`/`getSegments` are bound) with the entity index,
  keep them in `ChainRecord`, and give `shapeEntities` a chain-aware
  fallback. Contact events additionally need per-segment
  `Shape.enableContactEvents`/`enableHitEvents` (bound; the engine
  hard-codes both off in `b2CreateChain`, so registry stamping alone
  won't surface them). Doable today with N per-segment calls; a
  chain-level flag upstream would shrink it (NOTES-upstream §7). (M)
- [x] **`moveCharacter` per-iteration overhead.** Each of the up-to-5
  step iterations re-wraps the plane-visit `FunPtr` and allocates a fresh
  planes `IORef`, and the gather runs an O(n) `length` per plane result
  to enforce `planeCapacity` (a counter, or one `take planeCapacity` at
  the end, suffices). Hoist a single wrapper around the step loop. (S)
- [ ] **Event reader allocation.** The per-frame event globals
  (`Collisions`, `CollisionsEnd`, `SensorEvents`, `JointEvents`, `Moved`)
  build `VS.toList` + `[Maybe a]` + `catMaybes` intermediates and re-read
  the shape-registry `IORef` per event; snapshot the registry once per
  read and fold the storable vector directly. While there: consider
  splitting `SensorEvents` into begin/end globals mirroring
  `Collisions`/`CollisionsEnd`, so a reader pays only for the buffer it
  consumes. (S/M)
- [ ] **Registry scans on body destroy.** `Body`'s `explDestroy` rebuilds
  the whole shape/joint/chain maps with `IM.filter` per destroyed body,
  making a despawn wave O(bodies × records). A per-body reverse index —
  or at least skipping the rebuild when nothing matches — fixes the
  shape. (M)
- [ ] **Dedup the user-index resolvers.** `shapeEntities`, `jointEntity`
  and `bodyEntity` repeat the same validate/getUserIndex/registry/identity
  check (and its raw-API-user-index caveat comment) six times across the
  two packages; one generic resolver over `IORef (IntMap r)` with an id
  projection keeps the subtle index-0 guard in one place. Same direction:
  `toSensorEvent` duplicates `toCollision`'s two-shape resolution, and
  `axisBaseAt`/`wheelBaseAt` re-fill the four jointDef base fields
  `baseAt` owns. (S each)
- [ ] **`initPhysicsWith` store bootstrap.** The WorldDef-at-creation
  remainder of the P3 world-init item: a path to build the `Physics`
  store around a caller-supplied `WorldDef` (task system, capacities,
  bounds) — or a caller-supplied `WorldId`, which is what wrapping 2D
  `createFromSnapshot` needs too. All types are bound; this is wrapper
  design work, not FFI. (M)
- [ ] **Pre-solve callback (one-way platforms).** Carried from P3;
  `withPreSolveFcn` is bound, the work is FunPtr lifetime tied to the
  space. Still gated on a concrete demo driving it. (M)

### Blocked on upstream — do the box-nd side first (see NOTES-upstream.md)

- [x] **`overlapShape` + `castShape` queries.** No way to build a
  `ShapeProxy` from Haskell (opaque tag, no constructor or size).
  NOTES-upstream §1. (S here once unblocked)
  *2D done as `overlapQuery`/`sweepQuery` over `withShapeProxy` from
  box-nd 33ecce4. 3D still blocked: the 3D `ShapeProxy` remains an
  opaque tag.*
- [x] **Manifold/contact data on `Collisions`.** `ContactBeginTouchEvent`
  carries only ids + a `ContactId`, and `Contact.getData` writes into an
  opaque `ContactData`. NOTES-upstream §2. (S–M here once unblocked)
- [x] **[3D] User-authored meshes and height fields.** `GeoMesh`/
  `GeoHeightField` currently only accept the procedural generators;
  loading real level geometry needs constructible `MeshDef`/
  `HeightFieldDef`. NOTES-upstream §3. (S here once unblocked)
- [x] **[3D] `GeoCompound`.** `CompoundDef` and its child defs are opaque.
  NOTES-upstream §4. (M here once unblocked)
- [x] **[3D] Exact containment via the engine.** `containsPointQuery`
  special-cases spheres and capsules analytically because
  `getClosestPoint`'s `useRadii` mode keeps witness points on the
  perimeter even when overlapped; an `hsg_b3Shape_TestPoint` shim gives
  the engine's own answer and deletes the dispatch. NOTES-upstream §5.
  (S here once unblocked)
- [x] **Delete the hand-ported mover solver.** `solveMoverPlanes`/
  `clipMoverVector` + vec helpers are duplicated verbatim across the two
  packages only because `CollisionPlane`/`PlaneSolverResult` are opaque,
  making the engine's `solvePlanes`/`clipVector` uncallable. Binding them
  (NOTES-upstream §6) lets both ports be deleted — strictly better than
  the review's shared-module suggestion, which remains the fallback if
  upstream stalls. (S here once unblocked; M for the fallback)

## P4 — Deliberately not wrapping (escape hatch is the API)

- **Debug draw** (`World.draw` + DebugDraw struct): apecs-gloss/-3d already
  render shapes from components; big callback surface for little gain.
- **Standalone toolkits**: `DynamicTree`, `Collision.*` manifold/TOI/distance
  functions, `MathFunctions` — usable directly from the `Box2D`/`Box3D`
  modules without any apecs mediation; wrapping adds nothing.
- **Process-global hooks**: `Base` allocator/assert/log/timers,
  friction/restitution mixing callbacks, custom filter callback —
  application-level, not ECS-level.
- **`Internal`, `Tags`, `UserData`**: binding plumbing; the wrapper already
  owns the user-index channel (documenting *that* is a P1 docs task: raw-API
  users must not touch user indices or the registries break).

## Cross-cutting notes

1. **`pointQuery` broad-phase semantics** deserve a doc warning *now* even
   before the exact-query work lands (P2): apecs-physics users will misread it.
2. The wrapper's "reads return the exact value written" convention for
   `Shape`/`Joint` means engine-side mutation (via the P2 tuning components)
   must either update the stored record or document the divergence.
   *2026-07-06: this bit — the tuning components mutate the engine only,
   so a `Joint` re-set silently discards motor/limit settings. Registered
   as a review follow-up above. The matching `Shape`-side hole (sensor
   flag lost on recreate) is fixed in `carryMaterial`.*
3. [3D] default filter category is all-bits vs [2D] category 1 — already
   documented in `toQueryFilter`; keep that note when adding query APIs.
