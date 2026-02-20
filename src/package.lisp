;;;; package.lisp

(defpackage #:nuclear-mass
  (:use #:cl)
  (:local-nicknames (#:zippy #:org.shirakumo.zippy)
		    (#:jzon #:com.inuoe.jzon)
		    (#:sera #:serapeum))
  (:export
   :nucleus
   :*mass-table-path*
   :*mass-table*
   :+electron-mass+
   :+atomic-units->mev+
   :mev->atomic-units
   :atomic-units->mev
   :electron-binding-energy
   :measurement
   :make-nucleus
   :get-atomic-mass
   :get-nuclear-mass
   :mass-number
   :charge-number
   :name
   :atomic-mass
   :nuclear-mass
   :translate-nucleus-name
   :q-value
   :de-broglie-wavelength
   :reduced-mass
   :nuclear-radius
   :get-ground-state-spin
   :make-nucleus-levels
   :spin
   :excitation-energy
   :width-or-half-life
   :with-nuclear-masses
   :with-atomic-masses
   :find-levels
   :find-states-near-resonance
   :mass20
   :mass16
   :mass12
   :mass03
   :compare-masses
   :with-mass-table
   :ame->csv
   :update-ensdf
   :*ensdf-url*
   :*ensdf-json-url*
   :atomic-units->kev
   :gamow-peak
   :levels->csv
   :atomic-mass->nuclear-mass
   :value
   :uncertainty
   :mass-excess
   :nuclear-mass-excess
   :define-mass-table
   :level
   :excitation-unc
   :parsed-energy
   :parsed-unc
   :+electron-charge+
   :s-factor->cross-section
   :cross-section->s-factor
   :add-nuclei))
