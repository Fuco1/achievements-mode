# achievements-mode

Achievements for emacs!

Install, do `M-x achievements-mode` and earn your emacs badges. When you complete an achievement, message in echo area will alert you, it lokos like this:

    ACHIEVEMENT UNLOCKED: Foo. Check *achievements* for details.

Switch to buffer `*achievements*` to read the description and information about past levels of this achievement you've earned.

To list all earned achievements do `not-implemented-yet`.

# Contribute achievements!

Writing achievements is really easy, all it takes is one function and one call to register the achievement.

Write a function that is called `amode-achievement-foobar` and takes one (optional) argument `level` that contains the current level of the achievement or 0 if it wasn't earned yet. This function should return non-nil if achievement should be earned or nil if it shouldn't. The achievement handlers are run in the `post-command-hook`, so you can test for `this-command`, `last-command-event`, `current-prefix-arg` and so on. Make sure you return `nil` when the achievement shouldn't be earned! Emacs functions by default return value of the last form in their body.

Then call the macro `amode-add-achievement` to register the achievement into the system. Here's an example use:

```scheme
(amode-add-achievement
 "FOOBAR_ACHIEVEMENT" ;; ID of the achievement. Try to come up with a unique descriptive string
 "Do baz qux"         ;; general description
 3                    ;; number of levels
 ;; names of the levels
 ("First level name." "Second level n." "Third level n.")
 ;; descriptions of the levels
 ("First level description" "Second level desc." "Third level desc.")
 amode-achievement-foobar ;; handler name
 )
```

When you come up with a fun achievement for other users, please implement it and send a pull request! The more achievements we have, the more fun this will be! Contributing an achievement is also an achievement!

Achievements are defined in file `achievements-list.el`, so don't peek in or you can ruin yourself the fun! However, if you want to contribute, you should place the code there. You can also check the code to get inspiration if you don't know how to write the handlers.
