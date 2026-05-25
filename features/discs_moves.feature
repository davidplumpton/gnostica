Feature: Coins/Discs gameplay moves
  Disc moves should exercise the pure game-state transition through readable
  examples that cover piece growth, territory growth, resource costs, major
  variants, and rejection cases.

  Scenario: Grow an owned small piece and choose its orientation
    Given a Disc territory-source game with Rose's small minion at board index 3 facing east
    When Rose grows the Rose Disc piece with orientation south
    Then the Disc action succeeds
    And piece rose-medium-1 is on board index 3 facing south
    And the history records a :disc/piece-grown event
    And the game state is schema valid
    And every tarot card is accounted for exactly once

  Scenario: Grow an enemy piece without changing its orientation
    Given a Disc hand-card game with Rose's medium minion at board index 3 facing east and an Indigo target at board index 4 facing north
    When Rose grows the Indigo Disc piece
    Then the Disc action succeeds
    And piece indigo-medium-1 is on board index 4 facing north
    And the discard pile contains exactly "coins2"
    And Rose no longer has "coins2" in hand
    And the history records a :disc/piece-grown event
    And the game state is schema valid
    And every tarot card is accounted for exactly once

  Scenario: Reject an enemy piece reorientation
    Given a Disc hand-card game with Rose's medium minion at board index 3 facing east and an Indigo target at board index 4 facing north
    When Rose tries to grow the Indigo Disc piece with orientation west
    Then the Disc action is rejected with code :invalid-orientation
    And the previous state was not mutated

  Scenario: Grow a territory with a hand replacement
    Given a Disc hand-card territory-growth game
    When Rose grows the target territory with hand replacement "cupsking"
    Then the Disc action succeeds
    And board index 4 contains card "cupsking"
    And the discard pile contains exactly "coins2", "cups2"
    And Rose no longer has "coins2" in hand
    And Rose no longer has "cupsking" in hand
    And the history records a :disc/territory-grown event
    And the game state is schema valid
    And every tarot card is accounted for exactly once

  Scenario: Reject territory growth without a replacement card
    Given a Disc territory-source territory-growth game
    When Rose grows the target territory without a replacement
    Then the Disc action is rejected with code :invalid-disc-replacement
    And the previous state was not mutated

  Scenario: Reject an enemy-occupied territory target
    Given a Disc enemy-occupied territory-growth game
    When Rose grows the target territory with hand replacement "cupsking"
    Then the Disc action is rejected with code :target-territory-occupied-by-enemy
    And the previous state was not mutated

  Scenario: Reject a large piece target
    Given a Disc territory-source game with Rose's large minion at board index 3 facing east
    When Rose grows the Rose Disc piece
    Then the Disc action is rejected with code :target-piece-max-size
    And the previous state was not mutated

  Scenario: Reject growth when no larger stash piece is available
    Given a Disc no-medium-stash game with Rose's small minion at board index 3 facing east
    When Rose grows the Rose Disc piece
    Then the Disc action is rejected with code :no-larger-piece-available
    And the previous state was not mutated

  Scenario: Grow a royalty territory from Star after discarding the source card
    Given a Star hand-card territory-growth game
    When Rose grows the target territory from discard replacement "star"
    Then the Disc action succeeds
    And board index 4 contains card "star"
    And the discard pile contains exactly "cupsking"
    And Rose no longer has "star" in hand
    And the history records a :disc/territory-grown event
    And the game state is schema valid
    And every tarot card is accounted for exactly once

  Scenario: Use Strength to skip the intermediate piece size
    Given a Strength Disc shortcut game with Rose's small minion at board index 3 facing east
    When Rose uses Strength to grow the Rose Disc piece twice
    Then the Disc action succeeds
    And piece rose-large-1 is on board index 3 facing east
    And the discard pile contains exactly "strength"
    And Rose no longer has "strength" in hand
    And the history records a :disc/piece-grown event
    And the game state is schema valid
    And every tarot card is accounted for exactly once
