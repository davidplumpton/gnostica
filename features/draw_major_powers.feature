Feature: Draw and discard major powers

  Scenario: Fool can skip revealed draw-pile cards
    Given a Fool hand-card reveal game
    When Rose uses Fool to reveal 2 cards without playing them
    Then the draw-major action succeeds
    And Rose no longer has "fool" in hand
    And the discard pile contains exactly "fool", "cups2", "wands2"
    And every tarot card is accounted for exactly once
    And the game state is schema valid

  Scenario: High Priestess redraws twice while preserving card accounting
    Given a High Priestess hand-card redraw game
    When Rose uses High Priestess for two redraw passes
    Then the draw-major action succeeds
    And Rose has 5 cards in hand
    And Rose no longer has "high-priestess" in hand
    And the discard pile contains exactly "high-priestess", "cups2", "cups4"
    And every tarot card is accounted for exactly once
    And the game state is schema valid

  Scenario: Judgement rejects discard draws beyond the hand limit
    Given a Judgement hand-card limit game
    When Rose tries to draw too many cards with Judgement
    Then the draw-major action is rejected with code :invalid-judgement-card-count
    And the previous state was not mutated
    And every tarot card is accounted for exactly once
