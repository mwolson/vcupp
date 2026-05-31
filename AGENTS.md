# AGENTS.md

This repository is for an Emacs library (`vcupp`) that extends `use-package :vc`
support. It requires Emacs 30.1 or newer.

## Tips

- When making changes to data in existing code, try to keep things in
  alphabetical order when it's reasonable to do so.

## Planning

Prefer to write plans in the `plans/` directory.

## Dev loop tools

### Running checks

Run checks against the working tree (no staging required):

```sh
bun run hooks:check
```

This runs the pre-commit hooks against all working tree files with
`--all-files --no-stage-fixed`, so there is no stashing and no auto-staging.
Prefer this for iterating on changes before committing.

### Running tests

Run unit tests with:

```sh
npm run test
```

This executes the default ERT suite found in `test/test.el`.

### Checking for byte-compile warnings

Run the byte-compile check with:

```sh
npm run check
```

This byte-compiles all `.el` files and fails if there are any warnings. Fix all
warnings before committing.

### Checking for checkdoc warnings

Run checkdoc on each source file:

```sh
./scripts/checkdoc.sh vcupp.el
./scripts/checkdoc.sh vcupp-batch.el
./scripts/checkdoc.sh vcupp-native-comp.el
./scripts/checkdoc.sh vcupp-install-packages.el
```

All public and private functions must have docstrings.

### Quick Elisp sanity check

Check parentheses balance after edits:

```sh
emacs -Q --batch --eval '(progn (with-temp-buffer (insert-file-contents "vcupp.el") (check-parens)))'
```

### One-off batch harnesses

When iterating on a small part of the code, prefer writing a tiny one-off `.el`
file under `tmp/` and running it via `emacs -Q --batch`.

### MELPA recipe

The local source of truth for the MELPA recipe is `vcupp.recipe` at the
repository root. The Melpazoid CI workflow reads from that file.

## Releasing

### Pre-release steps

1. Check for uncommitted changes:

   ```sh
   git status
   ```

   If there are uncommitted changes, offer to commit them before proceeding.

2. Fetch latest tags to ensure we have the complete history:

   ```sh
   git fetch --tags
   ```

3. Update the version in `package.json` and `vcupp.el` (the `Version:` header),
   then commit the version bump separately from other changes with message
   `chore: bump version to <version>`.

4. Push the version-bump commit and verify CI passes before tagging:

   ```sh
   git push
   gh run watch          # wait for the check job to go green
   ```

   If CI fails, fix the issue and push again before proceeding.

5. Ask the user what tag name they want. Provide examples based on the current
   version:
   - If current version is `0.2.0`:
     - Minor update (new features): `0.3.0`
     - Bugfix update (patches): `0.2.1`

### Creating the release

When the user provides a version (or indicates major/minor/bugfix):

1. Create and push the tag:

   ```sh
   git tag v<version>
   git push origin v<version>
   ```

2. Examine each commit since the last tag to understand the full context:

   ```sh
   git log <previous-tag>..HEAD --oneline
   ```

   For each commit, run `git show <commit>` to see the full commit message and
   diff. Commit messages may be terse or only show the first line in `--oneline`
   output, so examining the full commit is essential for accurate release notes.

3. Create a draft GitHub release:

   ```sh
   gh release create v<version> --draft --title "v<version>" --generate-notes
   ```

4. Enhance the release notes with more context:
   - Use insights from examining each commit in step 2
   - Group related changes under descriptive headings (e.g., "### Refactored X",
     "### Fixed Y")
   - Avoid a single generic `## Changes` section when the release has multiple
     themes
   - Use bullet lists within each section to describe the changes
   - Include a brief summary of what changed and why it matters
   - Keep the "Full Changelog" link at the bottom
   - Update the release with `gh release edit v<version> --notes "..."`

   Ordering guidelines:
   - Put user-visible changes first (new features, bug fixes, breaking changes)
   - Put under-the-hood changes later (refactoring, internal improvements, docs)
   - Within each section, order by user impact (most impactful first)
   - Do not include routine verification sections or lists of check commands in
     public release notes; report validation in the chat handoff instead.

5. Tell the user to review the draft release and provide a link:

   ```
   https://github.com/mwolson/vcupp/releases
   ```

## Gotchas

### Emacs version requirements

vcupp requires Emacs 30.1. Several features it depends on (`use-package :vc`,
`package-vc`, `use-package-vc-valid-keywords`, `comp-run`) are not present in
earlier versions. The GitHub Actions CI workflow uses `purcell/setup-emacs` to
install Emacs 30.1 instead of the system Emacs package.

### Deprecated macros

- `when-let` and `if-let` are deprecated in favor of `when-let*` and `if-let*`.
  Always use the starred versions.

### Compile-time vs runtime evaluation

When silencing byte-compilation warnings about unknown functions or variables,
prefer `eval-when-compile` with `require` over `declare-function` or `defvar`.
Use forward `defvar` declarations (without a value) for variables defined in
packages that vcupp does not `require` at top level.
