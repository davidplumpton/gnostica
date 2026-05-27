Feature: Endgame scoring and challenge resolution
  The endgame rules score exclusively occupied territories and resolve a
  final-turn challenge as either victory or elimination.

  Scenario: A challenger wins after reaching the target score
    Given an endgame challenge game where Rose controls 9 points
    Then Rose has score 9
    When Rose announces a final-turn challenge
    Then the endgame action succeeds
    And Rose has an unresolved challenge
    When Indigo ends the turn
    Then the endgame action succeeds
    When Rose ends the turn
    Then the endgame action succeeds
    And the game winner is Rose by challenge
    And the game state is schema valid

  Scenario: A failed challenge eliminates the challenger
    Given an endgame challenge game where Rose controls 3 points
    Then Rose has score 3
    When Rose announces a final-turn challenge
    Then the endgame action succeeds
    And Rose has an unresolved challenge
    When Indigo ends the turn
    Then the endgame action succeeds
    When Rose ends the turn
    Then the endgame action succeeds
    And Rose is eliminated
    And Rose has no pieces on the board
    And Rose has 0 cards in hand
    And the game winner is Indigo by last-active-player
    And the game state is schema valid
