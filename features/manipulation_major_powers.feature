Feature: Manipulation major powers
  Hierophant, Hermit, and Devil moves should exercise the pure game-state
  transitions through readable examples that cover replacement, relocation,
  and retargeted orientation.

  Scenario: Hierophant replaces a target piece with Rose's same-size piece
    Given a Hierophant hand-card replacement game
    When Rose replaces the targeted piece with Hierophant facing south
    Then the manipulation-major action succeeds
    And piece rose-small-1 is on board index 4 facing south
    And Rose no longer has "hierophant" in hand
    And the discard pile contains exactly "hierophant"
    And the history records a :hierophant/piece-replaced event
    And the game state is schema valid

  Scenario: Hermit moves a targeted piece to an empty territory
    Given a Hermit hand-card piece-relocation game
    When Rose moves the Hermit target piece to board index 0
    Then the manipulation-major action succeeds
    And piece indigo-manipulation-target is on board index 0 facing north
    And Rose no longer has "hermit" in hand
    And the discard pile contains exactly "hermit"
    And the history records a :hermit/piece-moved event
    And the game state is schema valid

  Scenario: Hermit moves a targeted territory to an eligible wasteland
    Given a Hermit hand-card territory-relocation game
    When Rose moves the Hermit target territory to wasteland row 1 col 3
    Then the manipulation-major action succeeds
    And board index 4 is at row 1 col 3 with orientation portrait
    And piece rose-hermit-passenger is in wasteland row 1 col 1 facing north
    And there is no territory at row 1 col 1
    And Rose no longer has "hermit" in hand
    And the discard pile contains exactly "hermit"
    And the history records a :hermit/territory-moved event
    And the game state is schema valid

  Scenario: Devil retargets after orienting the acting minion
    Given a Devil territory-source retargeting game
    When Rose uses Devil to orient the minion and then the enemy target
    Then the manipulation-major action succeeds
    And piece rose-devil-minion is on board index 4 facing east
    And piece indigo-devil-target is on board index 5 facing south
    And the history records a :devil/piece-oriented event
    And the game state is schema valid
