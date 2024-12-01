
# Table of Contents

1.  [nuclear-mass](#orga3ac9a1)
    1.  [Examples](#org98079ec)
2.  [License](#org166992a)


<a id="orga3ac9a1"></a>

# nuclear-mass

This common lisp system includes tools that should make calculations involving
nuclear masses as painless as possible.


<a id="org98079ec"></a>

## Examples

    (ql:quickload :nuclear-mass)
    (nuclear-mass:get-atomic-mass "23na")

    (nuclear-mass:get-nuclear-mass "23na") ;; subtracts binding energy

You can change to different versions of AME:

    (reduce #'-
            (nuclear-mass:with-mass-table (nuclear-mass:mass93 nuclear-mass:mass20)
      (nuclear-mass:atomic-units->kev (nuclear-mass:get-atomic-mass "20mg"))))

Indicating that the mass of $^{20}$Mg was 93 keV greater in AME 1993 compared to 2020.

ENSDF is also included, and is automatically updated whenever the package is loaded.

    (nuclear-mass:get-ground-state-spin "23na") 

You can look for levels that match a lab-frame resonance energy.

    (mapc (lambda (level)
              (print level))
            (nuclear-mass:find-states-near-resonance 170.0 "1h" "23na" :delta 20.0))

**AND MORE!**


<a id="org166992a"></a>

# License

GPL v3

