Feature: Cups gameplay moves
  Cup moves should exercise target-space rules, small-piece creation,
  wasteland territory creation, and variant behavior through readable examples.

  Scenario: Reject a target outside the minion target space
    Given a Cup territory-source game with Rose's minion at board index 3 facing north and an Indigo target at board index 4 facing west
    When Rose creates a Cup piece on target board index 4 facing east
    Then the Cup action is rejected with code :cup-target-out-of-range
    And the previous state was not mutated

  Scenario: Create an enemy small piece by targeting an enemy piece
    Given a Cup territory-source game with Rose's minion at board index 3 facing east and an Indigo target at board index 4 facing west
    When Rose creates a Cup piece by targeting the Indigo piece
    Then the Cup action succeeds
    And piece indigo-small-1 is on board index 4 facing west
    And the history records a :cup/enemy-small-piece-created event
    And the game state is schema valid
    And every tarot card is accounted for exactly once

  Scenario: Create a wasteland territory from a hand card
    Given a Cup hand-card wasteland game with Rose's minion at board index 3 facing west
    When Rose creates a Cup territory in wasteland row 1 col -1 with hand card "coins2"
    Then the Cup action succeeds
    And board index 9 is at row 1 col -1 with orientation portrait
    And board index 9 contains card "coins2"
    And the discard pile contains exactly "cups2"
    And Rose no longer has "cups2" in hand
    And Rose no longer has "coins2" in hand
    And the history records a :cup/territory-created event
    And the game state is schema valid
    And every tarot card is accounted for exactly once

  Scenario: Reject a plain Cup target on a full territory
    Given a Cup full-target game
    When Rose creates a Cup piece on target board index 4 facing up
    Then the Cup action is rejected with code :target-territory-full
    And the previous state was not mutated

  Scenario: Empress ignores the full-territory Cup limit
    Given an Empress Cup full-target game
    When Rose creates a Cup piece on target board index 4 facing up
    Then the Cup action succeeds
    And piece rose-small-1 is on board index 4 facing up
    And the history records a :cup/small-piece-created event
    And the game state is schema valid
    And every tarot card is accounted for exactly once

  Scenario: Wheel creates a wasteland territory from the draw pile
    Given a Wheel Cup draw-pile wasteland game with Rose's minion at board index 3 facing west
    When Rose creates a Cup territory in wasteland row 1 col -1 from the draw pile
    Then the Cup action succeeds
    And board index 9 is at row 1 col -1 with orientation portrait
    And board index 9 contains the recorded draw-pile Cup card
    And the history records a :cup/territory-created event
    And the game state is schema valid
    And every tarot card is accounted for exactly once
