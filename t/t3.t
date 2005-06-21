use strict;
use Test::More qw(no_plan);
use IO::Capture::Stdout;

my $TM_ok = *Test::More::ok;

  # Pass any command line argument to have this test print the
  # Test::Usage output.
my $g_show_invoc = $ARGV[0];

# --------------------------------------------------------------------
# We will run these tests using Test::Usage and see if we get the
# expected output.

package lets_test;
use Test::Usage;

my $t = t();

$TM_ok->($t->time_took() == 0);
$TM_ok->($t->nb_fail() == 0);
$TM_ok->($t->nb_succ() == 0);

example('aa', sub {
  sleep 1;
  ok(1, q|'1' should succeed.|, 'But it failed.');
  ok(0, q|'0' should fail.|,    'And it did.');
});

example('b1', sub { ok(1, q|'1' should succeed.|, 'But it failed.') });
example('b2', sub { ok(1, q|'1' should succeed.|, 'But it failed.') });
example('b3', sub { ok(0, q|'0' should fail.|,    'And it did.') });
example('b4', sub { ok(1, q|'1' should succeed.|, 'But it failed.') });
example('b5', sub { ok(0, q|'0' should fail.|,    'And it did.') });

example('a2', sub {
  ok(1, q|'1' should succeed.|, 'But it failed.');
  ok(0, q|'0' should fail.|,    'And it did.');
  ok(2, q|'2' should succeed.|, 'But it failed.');
});

# --------------------------------------------------------------------
package main;

sub show_time_took     { print ">> took ", $t->time_took(), "\n" }

# These subs take as arguments a string and an expected number of
# occurences of something the sub will search for in the string. The
# sub will invoke Test::More's ok() to test whether the expected
# number of occurrences were found in the output produced by
# Test::Usage.

my %counting_subs = (
  ok_lines => sub {
    my ($str, $nb_exp) = @_;
    return _what_lines($str, '(?<!not )ok', $nb_exp);
  },
  not_ok_lines => sub {
    my ($str, $nb_exp) = @_;
    return _what_lines($str, 'not ok', $nb_exp);
  },
  summary_succ => sub {
    my ($str, $nb_exp) = @_;
    return _summary_what($str, '+', $nb_exp);
  },
  summary_fail => sub {
    my ($str, $nb_exp) = @_;
    return _summary_what($str, '-', $nb_exp);
  },
);

sub _what_lines {
    # $what is one of 'ok' or 'not ok'.
  my ($str, $what, $nb_exp) = @_;
  my $nb_got = () = $str =~ /$what /mg;
  ok($nb_got == $nb_exp, "Expecting $nb_exp $what.")
      or diag("$str : But got $nb_got.");
}

sub _summary_what {
    # $what is one of '+' or '-'.
  my ($str, $what, $nb_exp) = @_;
    # It contains something like '(00h:00m:01s).
  my ($summary_line) = $str =~ /^(.*\(.*\).*)$/m;;
  my ($nb_got) = $summary_line =~ /#.*?\Q$what\E(\d+).*/;
  $nb_got = -1 unless defined $nb_got;
  ok($nb_got == $nb_exp,
      "Expecting summary to show '$what$nb_exp'.")
      or diag("But it did not: '$summary_line'\n");
}

# --------------------------------------------------------------------
my $capture_stdout = IO::Capture::Stdout->new();

my $try = sub {
    # Expected counts for data appearing in the output.
  my (
      # A string representing arguments to pass to $t->test(...),
    $test_args,
      # Keys are a %counting_subs and values are expected return of
      # applying .
    %exp
  ) = @{$_[0]};
  my $args_str = join ', ',
      map { "$_ => '$test_args->{$_}'" } keys %$test_args;
    # Color may interfere with parsing of captured IO.
  t()->{options}{c} = 0;
  my $invoc = "test($args_str)";
  if ($g_show_invoc) {
    $DB::single = 1;
    print "---------- $invoc\n";
    eval $invoc;
  }
  else {
    $capture_stdout->start();
    eval $invoc;
    $capture_stdout->stop();
    my $stdout = join '', $capture_stdout->read();
    $counting_subs{$_}->($stdout, $exp{$_}) for keys %exp;
  }
};

# --------------------------------------------------------------------
# Here come the tests.

$try->([
  {a => 'aa', v => 0},
  ok_lines     => 0,
  not_ok_lines => 0,
  summary_succ => 1,
  summary_fail => 1,
]);

$TM_ok->($t->time_took() <= 1, 'f1') or show_time_took();
$TM_ok->($t->nb_fail() == 1, 'f2');
$TM_ok->($t->nb_succ() == 1, 'f3');

$try->([
  {a => 'aa', v => 0},
  ok_lines     => 0,
  not_ok_lines => 0,
  summary_succ => 1,
  summary_fail => 1,
]);

$TM_ok->($t->time_took() <= 1, 'g1') or show_time_took();
$TM_ok->($t->nb_fail() == 1, 'g2');
$TM_ok->($t->nb_succ() == 1, 'g3');

$try->($_) for (
  [ {v => 0},
    ok_lines     => 0,
    not_ok_lines => 0,
    summary_succ => 6,
    summary_fail => 4,
  ],
  [ {},
    ok_lines     => 0,
    not_ok_lines => 4,
    summary_succ => 6,
    summary_fail => 4,
  ],
  [ {v => 2},
    ok_lines     => 6,
    not_ok_lines => 4,
    summary_succ => 6,
    summary_fail => 4,
  ],
  [ {a => 'aa'},
    ok_lines     => 0,
    not_ok_lines => 1,
    summary_succ => 1,
    summary_fail => 1,
  ],
  [ {a => 'a*'},
    ok_lines     => 0,
    not_ok_lines => 2,
    summary_succ => 3,
    summary_fail => 2,
  ],
  [ {a => 'b*', v => 0},
    ok_lines     => 0,
    not_ok_lines => 0,
    summary_succ => 3,
    summary_fail => 2,
  ],
  [ {fail => 1},
    ok_lines     => 0,
    not_ok_lines => 10,
    summary_succ => 0,
    summary_fail => 10,
  ],
);

