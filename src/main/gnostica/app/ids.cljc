(ns gnostica.app.ids)

(def initialize :gnostica.app/initialize)
(def install-keyboard-shortcuts :gnostica.app/install-keyboard-shortcuts)
(def uninstall-keyboard-shortcuts :gnostica.app/uninstall-keyboard-shortcuts)
(def add-lobby-player :gnostica.app/add-lobby-player)
(def remove-lobby-player :gnostica.app/remove-lobby-player)
(def set-lobby-player-name :gnostica.app/set-lobby-player-name)
(def set-lobby-player-colour :gnostica.app/set-lobby-player-colour)
(def set-lobby-target-score :gnostica.app/set-lobby-target-score)
(def start-lobby-game :gnostica.app/start-lobby-game)
(def start-lobby-bidding :gnostica.app/start-lobby-bidding)
(def select-lobby-bid-card :gnostica.app/select-lobby-bid-card)
(def reveal-lobby-bids :gnostica.app/reveal-lobby-bids)
(def select-lobby-redraw-card :gnostica.app/select-lobby-redraw-card)
(def confirm-lobby-bidding :gnostica.app/confirm-lobby-bidding)
(def cancel-lobby-bidding :gnostica.app/cancel-lobby-bidding)
(def select-board-card :gnostica.app/select-board-card)
(def select-move-source :gnostica.app/select-move-source)
(def select-move-piece :gnostica.app/select-move-piece)
(def select-move-hand-card :gnostica.app/select-move-hand-card)
(def select-move-wasteland-target :gnostica.app/select-move-wasteland-target)
(def select-move-one-point-card :gnostica.app/select-move-one-point-card)
(def select-move-territory-card-source :gnostica.app/select-move-territory-card-source)
(def select-move-replacement-card :gnostica.app/select-move-replacement-card)
(def select-move-power :gnostica.app/select-move-power)
(def select-move-world-copy :gnostica.app/select-move-world-copy)
(def select-move-rod-mode :gnostica.app/select-move-rod-mode)
(def select-move-disc-target-kind :gnostica.app/select-move-disc-target-kind)
(def select-move-sword-target-kind :gnostica.app/select-move-sword-target-kind)
(def set-move-disc-action-count :gnostica.app/set-move-disc-action-count)
(def set-move-major-action-count :gnostica.app/set-move-major-action-count)
(def set-move-sword-action-count :gnostica.app/set-move-sword-action-count)
(def set-move-devil-action-count :gnostica.app/set-move-devil-action-count)
(def set-move-fool-reveal-count :gnostica.app/set-move-fool-reveal-count)
(def reveal-move-fool-card :gnostica.app/reveal-move-fool-card)
(def skip-move-fool-reveal :gnostica.app/skip-move-fool-reveal)
(def play-move-fool-reveal :gnostica.app/play-move-fool-reveal)
(def select-move-fool-play-power :gnostica.app/select-move-fool-play-power)
(def set-move-high-priestess-redraw-count :gnostica.app/set-move-high-priestess-redraw-count)
(def toggle-move-high-priestess-discard-card :gnostica.app/toggle-move-high-priestess-discard-card)
(def set-move-high-priestess-draw-count :gnostica.app/set-move-high-priestess-draw-count)
(def toggle-move-judgement-card :gnostica.app/toggle-move-judgement-card)
(def set-move-minion-orientation :gnostica.app/set-move-minion-orientation)
(def select-move-sun-disc-mode :gnostica.app/select-move-sun-disc-mode)
(def set-move-sun-disc-orientation :gnostica.app/set-move-sun-disc-orientation)
(def select-move-target-piece :gnostica.app/select-move-target-piece)
(def set-move-orientation :gnostica.app/set-move-orientation)
(def set-move-distance :gnostica.app/set-move-distance)
(def set-move-damage :gnostica.app/set-move-damage)
(def set-move-draw-count :gnostica.app/set-move-draw-count)
(def toggle-move-discard-card :gnostica.app/toggle-move-discard-card)
(def confirm-move :gnostica.app/confirm-move)
(def cancel-move :gnostica.app/cancel-move)
(def start-gesture-intent :gnostica.app/start-gesture-intent)
(def cancel-gesture-intent :gnostica.app/cancel-gesture-intent)
(def open-gesture-detailed-entry :gnostica.app/open-gesture-detailed-entry)
(def set-gesture-drag-orientation :gnostica.app/set-gesture-drag-orientation)
(def set-pending-placement-orientation
  :gnostica.app/set-pending-placement-orientation)
(def start-keyboard-placement-targeting
  :gnostica.app/start-keyboard-placement-targeting)
(def move-keyboard-placement-target
  :gnostica.app/move-keyboard-placement-target)
(def accept-keyboard-placement-target
  :gnostica.app/accept-keyboard-placement-target)
(def set-detailed-entry-default :gnostica.app/set-detailed-entry-default)
(def end-turn :gnostica.app/end-turn)
(def announce-challenge :gnostica.app/announce-challenge)
(def toggle-card-icon-mode :gnostica.app/toggle-card-icon-mode)
(def toggle-panel :gnostica.app/toggle-panel)
(def set-panel-open :gnostica.app/set-panel-open)
(def open-hotkey-help :gnostica.app/open-hotkey-help)
(def close-hotkey-help :gnostica.app/close-hotkey-help)
(def open-icon-help :gnostica.app/open-icon-help)
(def close-icon-help :gnostica.app/close-icon-help)
(def close-help-dialogs :gnostica.app/close-help-dialogs)
(def clear-three-texture-errors :gnostica.app/clear-three-texture-errors)
(def three-texture-error :gnostica.app/three-texture-error)
(def three-renderer-error :gnostica.app/three-renderer-error)
(def refresh-three-runtime-status :gnostica.app/refresh-three-runtime-status)
(def game :gnostica.app/game)
(def setup-error :gnostica.app/setup-error)
(def lobby :gnostica.app/lobby)
(def board :gnostica.app/board)
(def pieces :gnostica.app/pieces)
(def selected-board-index :gnostica.app/selected-board-index)
(def current-player :gnostica.app/current-player)
(def game-status :gnostica.app/game-status)
(def card-zones :gnostica.app/card-zones)
(def three-texture-errors :gnostica.app/three-texture-errors)
(def three-runtime-status :gnostica.app/three-runtime-status)
(def direct-manipulation :gnostica.app/direct-manipulation)
(def selected-board-cell :gnostica.app/selected-board-cell)
(def selected-board-pieces :gnostica.app/selected-board-pieces)
(def move-selection :gnostica.app/move-selection)
(def gesture-intent :gnostica.app/gesture-intent)
(def pending-move-tray-view :gnostica.app/pending-move-tray-view)
(def move-source-options :gnostica.app/move-source-options)
(def move-prompt :gnostica.app/move-prompt)
(def move-ready? :gnostica.app/move-ready?)
(def move-control-groups :gnostica.app/move-control-groups)
(def move-action-ribbon :gnostica.app/move-action-ribbon)
(def move-piece-options :gnostica.app/move-piece-options)
(def move-hand-card-options :gnostica.app/move-hand-card-options)
(def move-source-board-options :gnostica.app/move-source-board-options)
(def move-target-board-options :gnostica.app/move-target-board-options)
(def move-target-wasteland-options :gnostica.app/move-target-wasteland-options)
(def move-one-point-card-options :gnostica.app/move-one-point-card-options)
(def move-territory-card-source-options :gnostica.app/move-territory-card-source-options)
(def move-power-options :gnostica.app/move-power-options)
(def move-power :gnostica.app/move-power)
(def move-world-copy-options :gnostica.app/move-world-copy-options)
(def move-world-copied-power-options :gnostica.app/move-world-copied-power-options)
(def move-world-copied-power :gnostica.app/move-world-copied-power)
(def move-rod-mode-options :gnostica.app/move-rod-mode-options)
(def move-disc-action-count-options :gnostica.app/move-disc-action-count-options)
(def move-major-action-count-options :gnostica.app/move-major-action-count-options)
(def move-major-action-count :gnostica.app/move-major-action-count)
(def move-sword-action-count-options :gnostica.app/move-sword-action-count-options)
(def move-devil-action-count-options :gnostica.app/move-devil-action-count-options)
(def move-sun-disc-mode-options :gnostica.app/move-sun-disc-mode-options)
(def move-fool-reveal-count-options :gnostica.app/move-fool-reveal-count-options)
(def move-fool-reveal-state :gnostica.app/move-fool-reveal-state)
(def move-fool-play-power-options :gnostica.app/move-fool-play-power-options)
(def move-fool-play-power :gnostica.app/move-fool-play-power)
(def move-high-priestess-redraw-count-options :gnostica.app/move-high-priestess-redraw-count-options)
(def move-high-priestess-redraw-options :gnostica.app/move-high-priestess-redraw-options)
(def move-judgement-card-options :gnostica.app/move-judgement-card-options)
(def move-judgement-card-maximum :gnostica.app/move-judgement-card-maximum)
(def move-disc-minion-orientation-required? :gnostica.app/move-disc-minion-orientation-required?)
(def move-disc-target-kind-options :gnostica.app/move-disc-target-kind-options)
(def move-sword-target-kind-options :gnostica.app/move-sword-target-kind-options)
(def move-legal-targets :gnostica.app/move-legal-targets)
(def move-preview :gnostica.app/move-preview)
(def move-target-piece-options :gnostica.app/move-target-piece-options)
(def move-distance-options :gnostica.app/move-distance-options)
(def move-damage-options :gnostica.app/move-damage-options)
(def move-rod-orientation-required? :gnostica.app/move-rod-orientation-required?)
(def move-disc-orientation-available? :gnostica.app/move-disc-orientation-available?)
(def move-sun-disc-orientation-available? :gnostica.app/move-sun-disc-orientation-available?)
(def move-sword-orientation-available? :gnostica.app/move-sword-orientation-available?)
(def move-replacement-card-options :gnostica.app/move-replacement-card-options)
(def move-orientation-options :gnostica.app/move-orientation-options)
(def move-discard-card-options :gnostica.app/move-discard-card-options)
(def draw-count-options :gnostica.app/draw-count-options)
(def card-icon-mode :gnostica.app/card-icon-mode)
(def open-panels :gnostica.app/open-panels)
(def hotkey-help-open? :gnostica.app/hotkey-help-open?)
(def icon-help-open? :gnostica.app/icon-help-open?)
(def app-view :gnostica.app/app-view)
(def lobby-view :gnostica.app/lobby-view)
(def header-view :gnostica.app/header-view)
(def board-view :gnostica.app/board-view)
(def card-zones-view :gnostica.app/card-zones-view)
(def territory-view :gnostica.app/territory-view)
(def move-panel-view :gnostica.app/move-panel-view)
(def help-dialogs-view :gnostica.app/help-dialogs-view)
(def shuffle-seed :gnostica.app/shuffle-seed)
(def three-runtime-detection :gnostica.app/three-runtime-detection)

(def event-ids
  [initialize
   install-keyboard-shortcuts
   uninstall-keyboard-shortcuts
   add-lobby-player
   remove-lobby-player
   set-lobby-player-name
   set-lobby-player-colour
   set-lobby-target-score
   start-lobby-game
   start-lobby-bidding
   select-lobby-bid-card
   reveal-lobby-bids
   select-lobby-redraw-card
   confirm-lobby-bidding
   cancel-lobby-bidding
   select-board-card
   select-move-source
   select-move-piece
   select-move-hand-card
   select-move-wasteland-target
   select-move-one-point-card
   select-move-territory-card-source
   select-move-replacement-card
   select-move-power
   select-move-world-copy
   select-move-rod-mode
   select-move-disc-target-kind
   select-move-sword-target-kind
   set-move-disc-action-count
   set-move-major-action-count
   set-move-sword-action-count
   set-move-devil-action-count
   set-move-fool-reveal-count
   reveal-move-fool-card
   skip-move-fool-reveal
   play-move-fool-reveal
   select-move-fool-play-power
   set-move-high-priestess-redraw-count
   toggle-move-high-priestess-discard-card
   set-move-high-priestess-draw-count
   toggle-move-judgement-card
   set-move-minion-orientation
   select-move-sun-disc-mode
   set-move-sun-disc-orientation
   select-move-target-piece
   set-move-orientation
   set-move-distance
   set-move-damage
   set-move-draw-count
   toggle-move-discard-card
   confirm-move
   cancel-move
   start-gesture-intent
   cancel-gesture-intent
   open-gesture-detailed-entry
   set-gesture-drag-orientation
   set-pending-placement-orientation
   start-keyboard-placement-targeting
   move-keyboard-placement-target
   accept-keyboard-placement-target
   set-detailed-entry-default
   end-turn
   announce-challenge
   toggle-card-icon-mode
   toggle-panel
   set-panel-open
   open-hotkey-help
   close-hotkey-help
   open-icon-help
   close-icon-help
   close-help-dialogs
   clear-three-texture-errors
   three-texture-error
   three-renderer-error
   refresh-three-runtime-status])

(def public-subscription-ids
  [pending-move-tray-view
   app-view
   lobby-view
   header-view
   board-view
   card-zones-view
   territory-view
   move-panel-view
   help-dialogs-view])

(def internal-subscription-ids
  [game
   setup-error
   lobby
   board
   pieces
   selected-board-index
   current-player
   game-status
   card-zones
   three-texture-errors
   three-runtime-status
   three-renderer-error
   direct-manipulation
   selected-board-cell
   selected-board-pieces
   move-selection
   gesture-intent
   move-source-options
   move-prompt
   move-ready?
   move-control-groups
   move-action-ribbon
   move-piece-options
   move-hand-card-options
   move-source-board-options
   move-target-board-options
   move-target-wasteland-options
   move-one-point-card-options
   move-territory-card-source-options
   move-power-options
   move-power
   move-world-copy-options
   move-world-copied-power-options
   move-world-copied-power
   move-rod-mode-options
   move-disc-action-count-options
   move-major-action-count-options
   move-major-action-count
   move-sword-action-count-options
   move-devil-action-count-options
   move-sun-disc-mode-options
   move-fool-reveal-count-options
   move-fool-reveal-state
   move-fool-play-power-options
   move-fool-play-power
   move-high-priestess-redraw-count-options
   move-high-priestess-redraw-options
   move-judgement-card-options
   move-judgement-card-maximum
   move-disc-minion-orientation-required?
   move-disc-target-kind-options
   move-sword-target-kind-options
   move-legal-targets
   move-preview
   move-target-piece-options
   move-distance-options
   move-damage-options
   move-rod-orientation-required?
   move-disc-orientation-available?
   move-sun-disc-orientation-available?
   move-sword-orientation-available?
   move-replacement-card-options
   move-orientation-options
   move-discard-card-options
   draw-count-options
   card-icon-mode
   open-panels
   hotkey-help-open?
   icon-help-open?])

(def subscription-ids
  [game
   setup-error
   lobby
   board
   pieces
   selected-board-index
   current-player
   game-status
   card-zones
   three-texture-errors
   three-runtime-status
   three-renderer-error
   direct-manipulation
   selected-board-cell
   selected-board-pieces
   move-selection
   gesture-intent
   pending-move-tray-view
   move-source-options
   move-prompt
   move-ready?
   move-control-groups
   move-action-ribbon
   move-piece-options
   move-hand-card-options
   move-source-board-options
   move-target-board-options
   move-target-wasteland-options
   move-one-point-card-options
   move-territory-card-source-options
   move-power-options
   move-power
   move-world-copy-options
   move-world-copied-power-options
   move-world-copied-power
   move-rod-mode-options
   move-disc-action-count-options
   move-major-action-count-options
   move-major-action-count
   move-sword-action-count-options
   move-devil-action-count-options
   move-sun-disc-mode-options
   move-fool-reveal-count-options
   move-fool-reveal-state
   move-fool-play-power-options
   move-fool-play-power
   move-high-priestess-redraw-count-options
   move-high-priestess-redraw-options
   move-judgement-card-options
   move-judgement-card-maximum
   move-disc-minion-orientation-required?
   move-disc-target-kind-options
   move-sword-target-kind-options
   move-legal-targets
   move-preview
   move-target-piece-options
   move-distance-options
   move-damage-options
   move-rod-orientation-required?
   move-disc-orientation-available?
   move-sun-disc-orientation-available?
   move-sword-orientation-available?
   move-replacement-card-options
   move-orientation-options
   move-discard-card-options
   draw-count-options
   card-icon-mode
   open-panels
   hotkey-help-open?
   icon-help-open?
   app-view
   lobby-view
   header-view
   board-view
   card-zones-view
   territory-view
   move-panel-view
   help-dialogs-view])

(def cofx-ids
  [shuffle-seed
   three-runtime-detection])
