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

  Scenario: Resolve the official starting bid after a tied minor round
    Given an official starting-bid game with a tied minor bid and Gold winning the rebid
    Then the game state is schema valid
    And Gold is the starting player
    And the starting bid history has 2 rounds
    And the bid redraw order is "Indigo", "Rose", "Gold"
    And each player has 6 cards in hand
    And every tarot card is accounted for exactly once

  Scenario Outline: Official starting bid ranking follows the tarot ladder
    Given an official starting-bid rank game where Rose bids "<rose-bid>" and Indigo bids "<indigo-bid>"
    Then the game state is schema valid
    And <winner> is the starting player
    And the winning bid card is "<winning-bid>" with <arcana> rank <rank>
    And the bid redraw order is "<redraw-first>", "<winner>"
    And each player has 6 cards in hand
    And every tarot card is accounted for exactly once

    Examples:
      | rose-bid  | indigo-bid  | winner | winning-bid | arcana | rank | redraw-first |
      | fool      | magician    | Indigo | magician    | major  | 1    | Rose         |
      | hangedman | justice     | Rose   | hangedman   | major  | 12   | Indigo       |
      | judgement | world       | Indigo | world       | major  | 21   | Rose         |
      | cupsqueen | swordsknight | Rose   | cupsqueen   | minor  | 13   | Indigo       |
      | cupsace   | swords2     | Indigo | swords2     | minor  | 2    | Rose         |
