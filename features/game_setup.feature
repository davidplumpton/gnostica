Feature: Initial game setup
  Gameplay rules need deterministic setup scenarios that can be read as
  Gherkin-style examples while still exercising the browser-free game state.

  Scenario Outline: Create a deterministic setup for <player-count> players
    Given a deterministic game with <player-count> players
    Then the game state is schema valid
    And there are <player-count> players
    And each player has 6 cards in hand
    And the board has 9 face-up territory cards
    And every tarot card is accounted for exactly once

    Examples:
      | player-count |
      | 2            |
      | 3            |
      | 4            |
      | 5            |
      | 6            |
