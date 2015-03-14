set -ex

ROOT_DIR="$(cd "$(dirname ${BASH_SOURCE[0]})" && cd .. && pwd)"
PANDOC="/home/harry/build/cabal-sandboxes/pandoc/.cabal-sandbox/bin/pandoc"
PERMUTATIONS_POST="_posts/2014-07-21-permutations-an-exercise.html"

pushd "$ROOT_DIR"

# literate haskell
cat > "$PERMUTATIONS_POST" <<END
---
layout: post
title: "Permutations: an exercise"
---
END

"$PANDOC" \
    --from=markdown+lhs \
    --to=html \
    blog/permutations-an-exercise/permutations.lhs \
    >> "$PERMUTATIONS_POST"

popd
