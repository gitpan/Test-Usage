use strict;
use FindBin qw($Bin);
use lib "$Bin/lib";
use Test::Usage;

files(
  d => "$Bin/lib",
  i => "$Bin/lib",
  v => 2,
  c => 0,
);

