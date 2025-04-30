(use-package mwheel
  :ensure nil
  :custom
  (scroll-conservatively 101)
  (scroll-step 1)
  (mouse-wheel-progressive-speed t)
  (mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (mouse-wheel-scroll-amount-horizontal 1)
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-flip-direction (eq window-system 'ns)))
