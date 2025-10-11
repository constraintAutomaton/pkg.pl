# Subdirectory Imports - Reproduction Case

This directory demonstrates and reproduces the relative path resolution issue that was fixed in commit 5a25692.

## The Problem

When loading Prolog files from subdirectories (e.g., `tests/`), Bakage's `pkg()` imports would fail with an `existence_error` because `scryer_path/1` returned `"scryer_libs"` as a **relative path**. This relative path got resolved from the loading file's directory context rather than the project root.

### Error Example (Before Fix)

```
error(existence_error(source_sink,".../tests/scryer_libs/packages/testing/scryer-manifest.pl"),open/4)
```

Notice it was looking for `scryer_libs` inside the `tests/` directory instead of at the project root.

## The Solution

The fix modifies `scryer_path/1` to:
1. Search upward from the current working directory to find `scryer-manifest.pl`
2. Return an **absolute path** to `scryer_libs` based on the project root
3. Maintain backward compatibility with the `SCRYER_PATH` environment variable

## Testing This Reproduction

This directory structure allows you to verify the fix works:

```bash
# From the repository root
cd tests/subdirectory-imports

# Install dependencies
../../bakage.pl install

# Run test from root of this example (works)
scryer-prolog tests/test_my_module.pl -g "run_tests, halt."

# Run test from within tests subdirectory (also works with fix!)
cd tests
scryer-prolog test_my_module.pl -g "run_tests, halt."
```

### Expected Results

Both commands should succeed and show:
```
Running tests in module user.
  test "greet succeeds" ... succeeded
```

## What This Demonstrates

- ✅ Single Bakage installation at project root works for all subdirectories
- ✅ Tests can be organized in subdirectories without copying bakage
- ✅ Works from any subdirectory depth
- ✅ No need for multiple `scryer_libs/` directories

## Before the Fix

Before the fix, you would need to:
1. Copy `bakage.pl` into the `tests/` subdirectory
2. Install dependencies separately in that subdirectory
3. Maintain multiple copies of `scryer_libs/`

This was error-prone, wasted disk space, and complicated project structure.
