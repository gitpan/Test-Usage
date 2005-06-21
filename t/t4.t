use strict;
use Test::Usage;
use IO::Capture::Stdout;
use FindBin qw($Bin);

sub try {
  my ($exp_summary, %options) = @_;
  my $capture_stdout = IO::Capture::Stdout->new();
  $capture_stdout->start();
  files(%options, t => {c => 0, v => 0});
  $capture_stdout->stop();
  my $got_summary = join '', $capture_stdout->read();
  if (scalar($got_summary =~ /\Q$exp_summary/)) {
    print "ok 1\n";
  }
  else {
    print "not ok 1\n";
    print "  # Expected summary to contain '$exp_summary'\n";
    print "  # But got  '$got_summary'";
  }
}

my @tries = (
  [
    'Total +3 -3 0d 0w (00h:00m:00s) in 3 modules',
    d   => $Bin,
    i   => "$Bin/../lib",
    i2  => $Bin,
      # No colors for these tests; on Windows, Win32::Console output
      # is not captured by IO::Capture (may be possible, but FIXME
      # later).
    c => 0,
  ],
);

printf "1..%d\n", scalar @tries;
try(@$_) for @tries;

