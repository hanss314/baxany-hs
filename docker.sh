#!/bin/sh
docker run -it --rm -v $(pwd):/workspace -w /workspace terrorjack/asterius ahc-link --input-hs Export.hs --browser --bundle --output-directory build
