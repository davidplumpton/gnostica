Feature: Swords gameplay moves
  Sword moves should exercise piece attacks, territory attacks, replacement
  validation, destruction, void handling, and Tower's discard-pile variant.

  Scenario: Shrink an enemy piece with a hand source
    Given a Sword hand-card piece-attack game with Rose's medium minion at board index 3 facing east and an Indigo large target at board index 4 facing north
    When Rose attacks the Indigo Sword piece for 1 damage
    Then the Sword action succeeds
    And piece indigo-medium-1 is on board index 4 facing north
    And the discard pile contains exactly "swords2"
    And Rose no longer has "swords2" in hand
    And the history records a :sword/piece-shrunk event
    And the game state is schema valid
    And every tarot card is accounted for exactly once

  Scenario: Shrink a royalty territory with a hand replacement
    Given a Sword hand-card territory-attack game
    When Rose attacks the target Sword territory for 1 damage with hand replacement "cups2"
    Then the Sword action succeeds
    And board index 4 contains card "cups2"
    And piece rose-sword-territory-guard is on board index 4 facing south
    And the discard pile contains exactly "swords2", "cupsking"
    And Rose no longer has "swords2" in hand
    And Rose no longer has "cups2" in hand
    And the history records a :sword/territory-shrunk event
    And the game state is schema valid
    And every tarot card is accounted for exactly once

  Scenario: Reject an invalid territory replacement
    Given a Sword hand-card territory-attack game
    When Rose attacks the target Sword territory for 1 damage with hand replacement "swordsking"
    Then the Sword action is rejected with code :invalid-sword-replacement-card
    And the previous state was not mutated

  Scenario: Destroying a territory returns voided pieces to stash
    Given a Sword territory-destruction game
    When Rose destroys the target Sword territory
    Then the Sword action succeeds
    And there is no territory at row 0 col 0
    And piece rose-sword-territory-guard is in wasteland row 0 col 0 facing south
    And piece rose-outboard-minion is not on the board
    And the discard pile contains exactly "cups2"
    And the history records a :sword/territory-destroyed event
    And the game state is schema valid
    And every tarot card is accounted for exactly once

  Scenario: Tower shrinks a major territory from a discard-pile replacement
    Given a Tower Sword discard-pile replacement game
    When Rose attacks the target Sword territory for 1 damage from discard replacement "cupsking"
    Then the Sword action succeeds
    And board index 4 contains card "cupsking"
    And the discard pile contains exactly "star"
    And the history records a :sword/territory-shrunk event
    And the game state is schema valid
    And every tarot card is accounted for exactly once
