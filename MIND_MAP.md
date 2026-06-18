# Mind Map Format - Self-Documentation

> **For AI Agents:** This mind map is your primary knowledge index. Start with the Gnostica project anchors [6-10], then follow links [N] to find what you need; consult format nodes [1-5] only when you need the mind-map rules. Always reference node IDs. When you encounter bugs, document your attempts in relevant nodes. When you make changes, update outdated nodes immediately—especially project overview nodes since they're your springboard. Add new nodes only for genuinely new concepts. Keep it compact (20-50 nodes typical). The mind map wraps every task: consult it, rely on it, update it. Never change the first 5 numbered points.

> **Project Navigation Note:** In this Gnostica file, nodes [1-5] are preserved generic format references, not the project's daily navigation hubs. For project work, start at Gnostica anchors [6-10]; any generic wording inside [1-5] about overview nodes describes the mind-map format in general, not this repository's project anchors.

[1] **Mind Map Format Overview** - A graph-based documentation format stored as plain text files where each node is a single line containing an ID, title, and inline references [2]. The format leverages LLM familiarity with citation-style references from academic papers, making it natural to generate and edit [3]. It serves as a superset structure that can represent trees, lists, or any graph topology [4], scaling from small projects (<50 nodes) to complex systems (500+ nodes) [5].

[2] **Node Syntax Structure** - Each node follows the format: `[N] **Node Title** - node text with [N] references inlined` [1]. Nodes are line-oriented, allowing line-by-line loading and editing by AI models [3]. The inline reference syntax `[N]` creates bidirectional navigation between concepts, with links embedded naturally within descriptive text rather than as separate metadata [1][4]. This structure is both machine-parseable and human-readable, supporting grep-based lookups for quick node retrieval [3].

[3] **Technical Advantages** - The format enables line-by-line overwriting of nodes without complex parsing [2], making incremental updates efficient for both humans and AI agents [1]. Grep operations allow instant node lookup by ID or keyword without loading the entire file [2]. The text-based storage ensures version control compatibility, diff-friendly editing, and zero tooling dependencies [4]. LLMs generate this format naturally because citation syntax `[N]` mirrors academic paper references they've seen extensively during training [1][5].

[4] **Graph Topology Benefits** - Unlike hierarchical trees or linear lists, the graph structure allows many-to-many relationships between concepts [1]. Any node can reference any other node, creating knowledge clusters around related topics [2][3]. The format accommodates cyclic references for concepts that mutually depend on each other, captures cross-cutting concerns that span multiple subsystems, and supports progressive refinement where nodes are added to densify understanding [5]. This flexibility makes it suitable as a universal knowledge representation format [1].

[5] **Scalability and Usage Patterns** - Small projects typically need fewer than 50 nodes to capture core architecture, data flow, and key implementations [1]. Complex topics or large codebases can scale to 500+ nodes by adding specialized deep-dive nodes for algorithms, optimizations, and subsystems [4]. The methodology includes a bootstrap prompt for generating initial mind maps from existing codebases automatically [1]. Scale is managed through overview nodes [1-5] that serve as navigation hubs, with detail nodes forming clusters around major concepts [3][4]. The format remains navigable at any scale due to inline linking and grep-based search [2][3].

[6] **Gnostica App Overview** - Local CLJ/CLJS app with pure game state, re-frame app state, Three.js/CSS board, moves, fixtures, and br/JJ flow [7][8].

[7] **Rules and Authority** - Rules live in `docs/rules.txt`; command schemas, source, tests, and direct-move spec own implementation details [11][13][16][17].

[8] **Gameplay State** - `gnostica.game-state` is the browser-free gameplay facade and pure transition boundary. Route namespace ownership through [22], setup and turns through [14], command dispatch through [21], and coverage through [17].

[9] **App State and UI** - `gnostica.app-state` is the app-db facade for ids, subscriptions, lobby, gestures, moves, move-facade export tables/macros, panels, card zones, and handlers [11][13].

[10] **Verification and Workflow** - Use `clojure -M:lint`, `-M:test`, and for browser work `-M:release` plus `-M:smoke`; use br and Jujutsu [17][19][20].

[11] **Architecture Reference** - `docs/architecture.md` is canonical subsystem prose; update it instead of duplicating text in entry docs [6][8][9][18].

[12] **Board and Rendering** - `board`, `board-layout`, `pieces`, `ui.board`, and `three-board` share stable board indexes and renderer data [9][11][15].

[13] **Move Selection and Commands** - `gnostica.move-selection` stages browser-free move choices and command payloads. Use [23] for UI routing, [16] for gesture contracts, [21] for full-card dispatch, and [17] for regression anchors.

[14] **Setup, Scores, and Turns** - `setup/create-game` composes creation, optional starting bids, and bid-card redraws. Deck, player, hand, piece, score, challenge, and endgame rules stay pure [7][8][22].

[15] **Fixtures and Smoke Modes** - `gnostica.fixtures` owns lobby/demo defaults, smoke data, query-param init; keep fixtures out of pure setup [9].

[16] **Direct Move Entry** - The direct move spec is the manipulation contract; gestures stage data and preserve `:game` until confirmation [9][13].

[17] **Tests and Features** - Tests live under `test/gnostica`, features under `features/`, and focused regression inventories live in `docs/architecture.md`. Smoke targets released assets by default or `SMOKE_URL` [8][10][14][15].

[18] **Documentation Ownership** - Docs: README quick start, AGENTS workflow, MIND_MAP navigation, architecture prose, and rules authority [6][7][10][11].

[19] **Issue Tracking** - br-only tracker: start with `br ready`/`br show`, sync with `br sync --flush-only`, and keep `.beads/embeddeddolt` regular [10][20].

[20] **Jujutsu Workflow** - Prefer Jujutsu; inspect-only and tracker-only work should not create/split changes, while normal edits get a clear description before scoped work [10][19].

[21] **Major Power Dispatch** - `major-power` owns `apply-card-power`; implemented methods stay with gameplay-family owners. `major` owns shared source charging and ordered sequencing [8][11][13][22].

[22] **Gameplay Namespace Map** - Architecture maps core helpers, setup/deck/turn/score, spatial math, command contracts, suit moves, full-card families, World, and matching tests [8][11][14][21].

[23] **Move UI Routing** - Move selection owns prompts, legal targets, staging, registry metadata, builders, confirmation, previews, and action ribbons. App state, events, controls, and renderers consume those facades [9][13][16].
