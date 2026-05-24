Feature: Rods gameplay moves
  Rod moves should exercise the pure game-state transition through readable
  examples that cover the implemented move, push, and rejection cases.

  Scenario: Move an owned Rod minion and choose its orientation
    Given a Rod territory-source game with Rose's medium minion at board index 3 facing east
    When Rose moves the Rod minion 2 spaces with orientation south
    Then the Rod action succeeds
    And piece rose-rod-minion is on board index 5 facing south
    And the history records a :rod/minion-moved event
    And the game state is schema valid
    And every tarot card is accounted for exactly once

  Scenario: Push an enemy piece without changing its orientation
    Given a Rod hand-card game with Rose's medium minion at board index 3 facing east and an Indigo target at board index 4 facing north
    When Rose pushes the Indigo piece 1 space
    Then the Rod action succeeds
    And piece indigo-rod-target is on board index 5 facing north
    And the discard pile contains exactly "wands2"
    And Rose no longer has "wands2" in hand
    And the history records a :rod/piece-pushed event
    And the game state is schema valid
    And every tarot card is accounted for exactly once

  Scenario: Reject an enemy piece reorientation
    Given a Rod hand-card game with Rose's medium minion at board index 3 facing east and an Indigo target at board index 4 facing north
    When Rose tries to push the Indigo piece 1 space with orientation west
    Then the Rod action is rejected with code :invalid-orientation
    And the previous state was not mutated

  Scenario: Reject distance beyond the minion pip count
    Given a Rod territory-source game with Rose's medium minion at board index 3 facing east
    When Rose moves the Rod minion 3 spaces
    Then the Rod action is rejected with code :invalid-rod-distance
    And the previous state was not mutated

  Scenario: Reject a full territory destination
    Given a Rod full-destination game
    When Rose moves the Rod minion 1 space
    Then the Rod action is rejected with code :target-territory-full
    And the previous state was not mutated

  Scenario: Reject a void piece destination
    Given a Rod territory-source game with Rose's medium minion at board index 2 facing east
    When Rose moves the Rod minion 2 spaces
    Then the Rod action is rejected with code :rod-destination-void
    And the previous state was not mutated

  Scenario: Push a territory into an own-piece wasteland
    Given a Rod territory-push game with Rose's minion at board index 4 facing east, a Rose piece on target board index 5, and a Rose piece in landing wasteland row 1 col 3
    When Rose pushes the target territory 1 space
    Then the Rod action succeeds
    And board index 5 is at row 1 col 3 with orientation portrait
    And there is no territory at row 1 col 2
    And piece rose-territory-passenger is in wasteland row 1 col 2 facing up
    And piece rose-landing-minion is on board index 5 facing west
    And the discard pile contains exactly "wands2"
    And Rose no longer has "wands2" in hand
    And the history records a :rod/territory-pushed event
    And the game state is schema valid
    And every tarot card is accounted for exactly once

  Scenario: Reject a territory push target occupied by enemies
    Given a Rod enemy-occupied territory-push game
    When Rose pushes the target territory 1 space
    Then the Rod action is rejected with code :target-territory-occupied-by-enemy
    And the previous state was not mutated

  Scenario: Reject a territory push landing wasteland occupied by enemies
    Given a Rod enemy-occupied landing-wasteland game
    When Rose pushes the target territory 1 space
    Then the Rod action is rejected with code :wasteland-occupied-by-enemy
    And the previous state was not mutated
