# Ping

Most of the documentation about this, including the different directions it
could go, is currently in Evernote in a note called "Ping Game", tagged with
"ecs", "game-programming", and "games".

This is mostly an attempt at building a game using a component - entity - system
design to see how it works in Elm.

Find and shoot targets. You can't see targets, and they can't see you, unless
you ping them using the 'e' key.

Move around with the WASD keys. Ping with the 'e' key. Use the mouse to shoot
your weapons.

# Building and running

This project used Elm 0.18.

TO build and run, use elm-reactor:
```
elm-reactor
```

Point your browser at http://localhost:8000/.

# Design ideas
## Time
Current component and system updates take time in both an absolute duration
since the program started, and as the amount of time since the last update
occurred. Both are useful for different reasons, and I can imagine a third, an
integer counting the number of updates, for example to mark when two items
started to overlap with one another.

For this reason it would be handy to have a type that wraps these all up.

```elm
type alias TimePoint =
    { absolute : Time
    , relative : Time
    , count : Int
    }
```

The count might not be necessary; the current system for distinguishing new
overlaps from older ones is to use two separate lists, and that works fine.

## Systems
It seems like my systems need to maintain state. State belongs in components.
I've defined systems to be some kind of update that causes different entities to
interact. It's likely that this can be captured in component updates those, as
long as the order in which updates occur is "correct".

One of the two systems I have now is for determining when pings begin to overlap
with other objects so that secondary pings can be created. If all objects that
interact with pings have a simple bounding circle component, then the
intersection of all of those bounding circles can be calculated and the
individual components can be updated with a list of entities they overlap with,
and the time that overlap started.

It might still be necessary to have a system that can go through and look for
newly overlapping entities in order to create new entities with all required
components though, since the component update only has access to information for
that one type of component.

The other system is also for determining when things overlap, but based on
axis-aligned bounding-boxes (AABB) instead of bounding circles. Same principle,
the component update can determine which components overlap and update them with
a list of other entities they overlap with. Later a system can run that
facilitates a specific interaction, for example applying damage to a target when
it overlaps with a weapon.
