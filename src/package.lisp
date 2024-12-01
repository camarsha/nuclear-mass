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
   :with-nuclear-masses
   :with-atomic-masses
   :find-levels
   :find-states-near-resonance
   :mass20
   :mass16
   :mass12
   :mass03
   :mass95
   :mass93
   :compare-masses
   :with-mass-table
   :ame->csv
   :update-ensdf
   :*ensdf-url*
   :*ensdf-json-url*
   :atomic-units->kev))
