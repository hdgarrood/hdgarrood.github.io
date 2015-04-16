set -ex

ROOT_DIR="$(cd "$(dirname ${BASH_SOURCE[0]})" && cd .. && pwd)"
PERMUTATIONS_POST="_posts/2014-07-21-permutations-an-exercise.html"

pushd "$ROOT_DIR"

# literate haskell
cat > "$PERMUTATIONS_POST" <<END
---
layout: post
title: "Permutations: an exercise"
---
END

pandoc \
    --from=markdown+lhs \
    --to=html \
    blog/permutations-an-exercise/permutations.lhs \
    >> "$PERMUTATIONS_POST"

popd

# literate haskell again
AESON_POST_SOURCE="_lhs/aeson-better-errors"
AESON_POST="_posts/2015-04-16-aeson-better-errors.html"

cat "${AESON_POST_SOURCE}.frontmatter" > "$AESON_POST"
pandoc \
    --from=markdown+lhs \
    --to=html \
    "${AESON_POST_SOURCE}.lhs" \
    >> "$AESON_POST"
