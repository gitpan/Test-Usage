package Template_T;

use strict;
use Test::Usage;
use Template;

example('a1', sub {
  ok(0,
    "Expected some tests to be defined.",
    "But there are none."
  );
});

1;

