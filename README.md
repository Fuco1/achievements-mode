# DEPRECATED

**This mode is now deprecated in favor of https://bitbucket.org/gvol/emacs-achievements (of which I was unaware). That package already has hundreds of achievements, nicer API and is in general more "worked out". I'm in contact with the author and we working on merging the missing features over.**

# achievements-mode

Achievements for emacs!

Install, do `M-x achievements-mode` and earn your emacs badges. When you complete an achievement, message in echo area will alert you, it lokos like this:

    ACHIEVEMENT UNLOCKED: Foo. Check *achievements* for details.

Switch to buffer `*achievements*` to read the description and information about past levels of this achievement you've earned.

To list all earned achievements do `not-implemented-yet`.

# Installation

The easiest way is to install this via `package.el` from MELPA repository. If you want to install manually, clone the git repo and add it to your `load-path`.

This package depends on `dash.el` [dash.el](https://github.com/magnars/dash.el) and `f.el` [f.el](https://github.com/rejeep/f.el). If you don't install this package from MELPA, then you'll need to put these two packages on your `load-path` manually as well. See their web pages for install information.

You should customize `achievements-mode` to enable itself on emacs startup so you won't miss any fun.

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

Each achievement can store a persistent state. You can access and mutate these data by  using the following functions:

* `(amode-get-achievement-data id)` - return the plist containing the persisting data for achievement `id`. This automatically contains `:level` attribute containing the current level of this achievement. To further query this plist, use `(plist-get plist :property)`.
* `(amode-set-achievement-data id prop data)` - set the property `prop` of achievement `id` to `data`.

Here's a complete example of an achievement that saves additional state and has multiple levels:

```scheme
(defun amode-achievement-arrow-in-a-row (&optional level)
  (when (memq last-command-event '(left right up down))
    (let* ((data (amode-get-achievement-data "ARROW_COMBO"))
           (repeat (or (plist-get data :repeat) 0)))
      (if (>= repeat (nth level '(5 10)))
          t
        (amode-set-achievement-data "ARROW_COMBO" :repeat (1+ repeat))
        nil))))

(amode-add-achievement
 "ARROW_COMBO"
 "Hit arrow keys several times in a row."
 2
 ("Sharpshooter." "Master marksman.")
 ("Hit the arrow key 5 times in a row."
  "Hit the arrow key 10 times in a row.")
 amode-achievement-arrow-in-a-row)
```

When you come up with a fun achievement for other users, please implement it and send a pull request! The more achievements we have, the more fun this will be! Contributing an achievement is also an achievement!

**Warning**: achievements are defined in file `achievements-list.el`, so don't peek in or you can ruin yourself the fun! However, if you want to contribute, you should place the code there. You can also check the code to get inspiration if you don't know how to write the handlers.
