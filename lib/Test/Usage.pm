package Test::Usage;

use 5.008;
our $VERSION = '0.04';

=head1 NAME

Test::Usage - Manage test usage example suites throughout module development.

=head1 SYNOPSIS

Let's say we are building module Foo.pm. To exercise it, we write a
test usage examples module, Foo_T.pm, which may eventually look
something like this:

  package Foo_T;
  use strict;
  use Test::Usage;
  use Foo;

  example('e1', sub { ...  ok(...); ...  ok(...); ... });
  example('a1', sub { ...  ok(...) or diag(...); ... });
  example('a2', sub { ...  ok(...); ... });
  example('a3', sub {
    my $f = Foo->new();
    my $got_foo = $f->foo();
    my $exp_foo = 'FOO';
    ok(
      $got_foo eq $exp_foo,
      "Expecting foo() to return '$exp_foo'.",
      "But got '$got_foo'."
    );
  });

Here are a few ways to test its examples:

  perl -MFoo_T -e 'test(a => "a3")' # Run example a3 only.
  perl -MFoo_T -e 'test(a => "a*")' # Run all a: a1, a2, a3.
  perl -MFoo_T -e 'test()'          # Run all: e1, a1, a2, a3.
  perl -MFoo_T -e 'test(a => "a3", v => 2, c => 0)' # See test().

To summarize tests ran on all "*_T.pm" files found under current
directory:

  perl -MTest::Usage -e files   # See files().

=head1 DESCRIPTION

This module helps Perl developers maintain and run test usage
examples.

Test files are usually paired with an ordinary Perl module, Foo.pm for
example. The test file is also a Perl module, conventionally named
Foo_T.pm, and also conventionally resides in the same directory as
Foo.pm (it's really part of the code, so I like it to stay close to
the code). If the test module is not associated to any module in
particular, that's okay too. The test module has a conventional
structure, like the one shown in the SYNOPSIS. Basically, it just
names the module, loads Test::Usage and defines a bunch of examples.

When Test::Usage is loaded, it determines which color mechanism to
apply (currently, ANSI or Win32Console) and it runs its import()
method, which sets up a you-might-call-it-a-singleton instance of
Test::Usage, setting its test runs counters to 0. It also exports some
of its methods to the calling package and some to main, to make them
easier to use.

Each example(), identified by its label,  adds to the tests that the
module can run, upon request.

When the developer wishes to run a test, he invokes it from the
command line (perhaps with a coating of shell syntaxic sugar), as
shown in the synopsis.

=cut

# --------------------------------------------------------------------
use strict;
use warnings;
use Carp;
$Carp::MaxArgLen = 0; # So error messages don't get truncated.
use File::Temp qw(tempfile);
use File::Find;
use File::Slurp;
use File::Spec;
use IO::File;

  # Main accumulator.
my $t;

# --------------------------------------------------------------------
# Color management.

my $gColor = {};

  # Explicit initializations, for the maintainer's benefit.
$gColor->{id} = undef;
$gColor->{palette} = undef;
  # Will be set only for Win32::Console usage.
$gColor->{out} = undef;

if ($^O eq 'MSWin32') {
  eval "use Win32::Console";
  if ($@ eq '') {
    $gColor->{out} = Win32::Console->new(STD_OUTPUT_HANDLE())
        or die "Couldn't get new Win32::Console instance.";
    $gColor->{palette} = {
      cBoldWhite    => $::FG_WHITE        | $::BG_BLACK,
      cBoldMagenta  => $::FG_LIGHTMAGENTA | $::BG_BLACK,
      cBoldCyan     => $::FG_LIGHTCYAN    | $::BG_BLACK,
      cYellow       => $::FG_YELLOW       | $::BG_BLACK,
      cBoldGreen    => $::FG_LIGHTGREEN   | $::BG_BLACK,
      cBoldRed      => $::FG_LIGHTRED     | $::BG_BLACK,
      cWhite        => $::FG_WHITE        | $::BG_BLACK,
      cBlack        => $::FG_BLACK        | $::BG_BLACK,
    };
    $gColor->{id} = 'Win32Console';
  }
}
else {
  eval "use Term::ANSIColor ()";
  if ($@ eq '') {
    $gColor->{palette} = {
      cBoldWhite    => 'bold white',
      cBoldMagenta  => 'bold magenta',
      cBoldCyan     => 'bold cyan',
      cYellow       => 'yellow',
      cBoldGreen    => 'bold green',
      cBoldRed      => 'bold red',
      cWhite        => 'white',
      cBlack        => 'black',
    };
    $gColor->{id} = 'ANSI';
  }
}

# --------------------------------------------------------------------
# Verbosity level constants.

sub REPORT_NOTHING  () { 0 }
sub REPORT_FAILURES () { 1 }
sub REPORT_ALL      () { 2 }

# --------------------------------------------------------------------
# Default options. Change if necessary.
#
# Implementation note: all leaf keys of %_D must be different, since
# they become keys to $t->{options}.

my %_D;
  # Can be set by test().
$_D{t} = {
    # Accept tests whose label matches this glob.
  a => '*',
    # Exclude tests whose label matches this glob.
  e => '__*',
    # Print a summary line if true.
  s => 1,
    # Verbosity level.
  v => REPORT_FAILURES,
    # Fail tests systematically if true.
  fail => 0,
};
  # Can be set by files().
$_D{f} = {
    # Directory in which to look for files.
  d => '.',
    # Test files whose name matches this glob.
  g => '*_T.pm',
    # Look for files recursively through dir if true.
  r => 1,
    # Add to Perl %INC path.
  i => '',
    # Option values to pass to test() for each file.
  t => {},
};
  # Miscellaneous. Can be set by test() or files().
$_D{m} = {
    # Use color if possible.
  c => 1,
};
  # Color map.
$_D{c} = {
  what    => 'cBoldWhite',
  died    => 'cBoldMagenta',
  warning => 'cBoldCyan',
  summary => 'cYellow',
  success => 'cBoldGreen',
  failure => 'cBoldRed',
  diag    => 'cBoldRed',
  default => 'cWhite',
};

# --------------------------------------------------------------------

=head1 METHODS AND FUNCTIONS

All methods apply to a single instance of Test::Usage, named $t,
initialized by import().

The module defines the following methods and functions.

=cut

# --------------------------------------------------------------------

=head2 import ($pkg)

Sets $t to an empty hash ref, blessed in Test::Usage.

Resets $t's counters to 0. See the corresponding methods:

  time_took()
  nb_succ()
  nb_fail()
  died()
  warned()

Sets $t's default label to '-'.

Resets $t's options to default values. Here are the as-shipped
values:

  For the test() method:

    a => '*'             # Accept tests whose label matches this glob.
    e => '__*'           # Exclude tests whose label matches this glob.
    s => 1               # Print a summary line if true.
    v => REPORT_FAILURES # Verbosity level.
    fail => 0            # Fail tests systematically if true.

  For the files() method:

    d => '.'      # Directory in which to look for files.
    g => '*_T.pm' # Test files whose name matches this glob.
    r => 1        # Look for files recursively through dir if true.
    i => ''       # Add to Perl %INC path.
    t => {}       # Option values to pass to test() for each file.

  For both test() and files():

    c => 1  # Use color if possible.

Exports these methods to the calling package:

  t
  example
  ok
  ok_labeled
  diag

Exports these methods to main:

  t
  test
  files
  labels

=cut

sub import {
  $t = bless {}, __PACKAGE__;
  my $caller = caller;
  $t->{name} = $caller;
    # example() will push elements like [$label, $sub_ref]
    # onto this array ref.
  $t->{examples}  = [];
    # Default label.
  $t->{label}     = '-';
    # Counters.
  $t->{time_took} = 0;
  $t->{nb_succ}   = 0;
  $t->{nb_fail}   = 0;
    # Incremented when an ok*() causes respectively a die or a
    # warning.
  $t->{died}      = 0;
  $t->{warned}    = 0;

  reset_options();

  eval << "EOT";
    package $caller;
    *t          = sub { \$t };
    *example    = sub { \$t->example(\@_) };
    *ok         = sub { \$t->ok(\@_) };
    *ok_labeled = sub { \$t->ok_labeled(\@_) };
    *diag       = sub { \$t->diag(\@_) };
EOT
  eval "*main::t = sub { \$t }" unless $caller eq 'main';
  eval << "EOT";
    package main;
    *test           = sub { \$t->test(\@_) };
    *files          = sub { \$t->files(\@_) };
    *labels         = sub { \$t->labels(\@_) };
EOT
 # }
}

# --------------------------------------------------------------------

=head2 $pkg::t (), ::t ()

Both return $t, effectively giving access to all Test::Usage methods.

=cut

# --------------------------------------------------------------------

=head2 $pkg::example ($label, $sub_ref)

Add a test example labeled $label and implemented by $sub_ref to the
tests that can be run by $t->test().

$label is an arbitrary string that is used to identify the example.
The label will be displayed when reporting tests results.  Labels can
be chosen to make it easy to run selected subsets; for example, you
may want to label a bunch of examples that you usually run together
with a common prefix.

The $sub_ref is a reference to the subroutine implementing the test.
It often calls a number of ok(), wrapped in setup/tear-down
scaffolding to express the intended usage.

Here's a full example:

  example('t1', sub {
    my $f = Foo->new();
    my $exp = 1;
    my $got = $f->get_val();
    ok(
      $got == $exp,
      "Expected get_val() to be $exp for a new Foo object.",
      "But got $got.",
    );
  });

=cut

sub example {
  my ($self, $label, $sub_ref) = @_;
    # We store test examples in an array to guarantee that they will
    # be executed in the order they appear in the test file.
  push @{$self->{examples}},
      Test::Usage::Example->new($label, $sub_ref);
}

# --------------------------------------------------------------------

=head2 $pkg::ok ($bool, $exp_msg, $got_msg)

$bool is an expression that will be evaluated as true or false, and
thus determine the return value of the method.  Also, if $bool is
true, $t will increment the number of successful tests it has seen,
else the number of failed tests.

Note that $bool will be evaluated in list context; for example, if you
want to use a bind operator here, make sure you wrap it with
'scalar'. For example:

  ok(scalar($x =~ /abc/),
    "Expected \$x to match /abc/.",
    "But its value was '$x'."
  );

In that example, if 'scalar' is not used, the bind operator is
evaluated in list context, and if there is no match, an empty list is
returned, which results in ok() receiving only the last two arguments.

If the test() flags are such that the result of the ok() is to be
printed, something like one of the following will be printed:

  ok a1
    # Expected $x to match /abc/.

  not ok a1
    # Expected $x to match /abc/.
    # But its value was 'def'.

ok() is most useful when it describes expected result and helps
debug when it fails. So the $exp_msg should tell the user what
the test is expecting, and the $got_msg, what it got instead.  It
is useful to formulate them in terms that are useful for
development, maintenance, and debugging.  Compare the following
examples:

=over 4

=item Useful

  ok(! defined($got = foo()),
    'foo() should return undef if no arguments are given.',
    "But returned '$got' instead."
  );

Whether it succeeds of fails, the following messages are helpful:

  ok a1
    # foo() should return undef if no arguments are given.

  not ok a1
    # foo() should return undef if no arguments are given.
    # But returned '' instead.

=item Useless

  ok(! defined(my $got = foo()),
    'Result is undefined.',
    'Didn\'t work.'
  );

Whether it succeeds of fails, we don't really know what exactly went
right, or wrong:

  ok a1
    # Result is undefined.

  not ok a1
    # Result is undefined.
    # Didn't work.

=back

=cut

sub ok {
  my ($self, $bool, $exp_msg, $got_msg) = @_;
  $self->_confirm($self->{label}, $bool, $exp_msg, $got_msg);
}

# --------------------------------------------------------------------

=head2 $pkg::ok_labeled ($sub_label, $bool, $exp_msg, $got_msg)

Same as ok(), except that ".$sub_label" is appended to the label in
the printed output. This is useful for examples containing many ok()
whose labels we want to distinguish.

=cut

sub ok_labeled {
  my ($self, $sub_label, $bool, $exp_msg, $got_msg) = @_;
  $self->_confirm(
      $self->{label} . '.' . $sub_label,
      $bool, $exp_msg, $got_msg);
}

# --------------------------------------------------------------------

=head2 $pkg::diag (@msgs)

Prefixes each line of each string in @msgs with '  # ' and displays
them using the 'diag' color tag. Returns true (contrary to
Test::Builder, Test::More, et al.).

=cut

sub diag {
  my($self, @msgs) = @_;
  return unless @msgs && $self->options()->{v} > 0;
    # Prefix each line to make it a comment, to avoid interference
    # when used with Test::Harness.
  foreach (@msgs) {
    $_ = 'undef' unless defined;
    s/^/  # /gm;
  }
  push @msgs, "\n" unless $msgs[-1] =~ /\n\Z/;
  $self->printk('diag', "@msgs");
  return 1;
}

# --------------------------------------------------------------------

=begin maint $self->_confirm ($label, $bool, $exp_msg, $got_msg)

Returns true if argument $bool is true, false otherwise. Also returns
false if the 'fail' parameter was passed to the constructor.

=end maint

=cut

sub _confirm {
  my ($self, $label, $bool, $exp_msg, $got_msg) = @_;
  $bool = 0 if $self->options()->{fail};
  ($bool && $self->_incr_nb_succ()) || $self->_incr_nb_fail();
  my $verbosity = $self->options()->{v};
  $exp_msg = '' unless defined $exp_msg;
  $got_msg = '' unless defined $got_msg;
  if ($verbosity == REPORT_ALL || ($verbosity == REPORT_FAILURES && ! $bool)) {
    $self->printk('what', $bool ? 'ok ' : 'not ok ');
    my $printk_type = $bool ? 'success' : 'failure';
    $self->printk($printk_type, "$label\n");
    if ($exp_msg ne '') {
      $exp_msg =~ s/^/  # /gm;
      $exp_msg =~ s/\n*$/\n/;
      $self->printk($printk_type, $exp_msg);
    }
    if (! $bool && $got_msg ne '') {
      $got_msg =~ s/^/  # /gm;
      $got_msg =~ s/\n*$/\n/;
      $self->printk($printk_type, $got_msg);
    }
  }
  return $bool;
}

# --------------------------------------------------------------------

=head2 ::labels ()

Returns a ref to an array holding the labels of all the examples, in
the order they were defined.

=cut

sub labels {
  [map {$_->{label}} @{$_[0]->{examples}}];
}

# --------------------------------------------------------------------

=head2 ::test (%options)

Run all the known examples, subject to the constraints indicated by
%options.  If %options is undefined or if some of its keys are
missing, default values apply.  Here is the meaning and default value
of the elements of %options:

=over 4

=item a => '*' # Accept.

The value is a glob. Tests whose label matches this glob will be run.
All tests are run when the value is the default.

=item e => '__*' # Exclude.

The value is a glob. Tests whose label matches this glob will not be
run. I use this when I want to keep a test in the test module, but I
don't want to run it for some reason. When using the default value,
prepending the string '__' to a test label will effectively
disactivate it. When you are ready to run those tests, remove the '__'
prefix from the label, or pass the 'e => ""' argument.

=item v => 1  # Verbosity.

Determines the verbosity of the testing mechanism:

  0: Display no individual results.
  1: Display individual results for failing tests only.
  2: Display individual results for all tests.

=item s => 1  # Summary.

If true, two lines like the following will wrap the test output:

  # module_name
    ...
    # +3 -1 -d +w (00h:00m:02s) module_name

That means that of the ok*() calls that were made, 3 succeeded and 1
failed, that no dies but some warnings occurred, and it took about 2
seconds to run.

=item fail => 0  # Fail.

If true, any ok*() invoked will act as though it failed. When combined
with a verbosity of 1 or 2, (to display failures), you will see all
the actual messages that would get printed when failures occur.

=back

=cut

{
  my $tee_hdl;

  sub tee_to {
    my ($self, $file_name) = @_;
    $tee_hdl = IO::File->new("> $file_name");
  }

  sub test {
    my ($self, %options) = @_;
    $self->{nb_succ} = $self->{nb_fail} = 0;
      # Run examples matching this glob.
    my $accept = glob_to_regex(
      defined($options{a})
        ? $options{a}
        : $self->{options}{a}
    );
      # Don't run examples matching this glob.
    my $exclude = glob_to_regex(
      defined($options{e})
        ? $options{e}
        : $self->{options}{e}
    );
    $self->{options}{c} = $options{c} if defined $options{c};
    $self->{options}{v} = _adjust_verbosity($options{v}) if defined $options{v};
    $self->{options}{fail} = $options{fail} if defined $options{fail};
    my $start_time = time;
      # Run the examples.
    $self->printk('summary', '# ' . $self->{name} . "\n") if $options{in_files};
    for my $example (@{$self->{examples}}) {
      my $label   = $example->{label};
      my $sub_ref = $example->{sub_ref};
      next unless $label =~ /$accept/;
      next if $label =~ /$exclude/;
      $self->{label} = $label;
      my $warnings = '';
      eval {
        local $SIG{__DIE__} = sub {
          Carp::confess();
        };
        local $SIG{__WARN__} = sub {
          $DB::single = 1;
          $warnings .= Carp::longmess(@_) . "\n";
        };
        $sub_ref->($self);
      };
      if ($warnings) {
        $self->printk('keyword', 'WARNED ');
        $self->printk('warning', $warnings);
        ++$self->{warned};
      }
      if ($@) {
        $self->printk('keyword', 'DIED ');
        $self->printk('died', $@);
        ++$self->{died};
      }
    }
    $self->{time_took} = time - $start_time;
    my $summary = sprintf "  # +%d -%d %sd %sw (%s) %s\n",
      $self->nb_succ(),
      $self->nb_fail(),
      ($self->died() ? '+' : '-'),
      ($self->warned() ? '+' : '-'),
      _elapsed_str($self->{time_took}),
      $self->{name},
    ;
    print $tee_hdl $summary if $tee_hdl;
    my $print_summary  = defined($options{s}) ? $options{s} : $self->{options}{s};
    if ($print_summary) {
      $self->printk('summary', sprintf "1..%d\n",
          $self->nb_succ() + $self->nb_fail()) unless $options{in_files};
      $self->printk('summary', $summary);
    }
    $self->reset_options();
  }

}

# --------------------------------------------------------------------

=head2 ::files (%options)

After having found all the files that correspond to the criteria
defined in %options (for example, directory to look in), for each file
calls perl in a subshell to run something like this:

    perl -M$file -e 'test()'

The results of each run are collected, examined and tallied, and a
summary line is displayed.

All values in %options are optional. Their meaning and default value
are as follows:

=over 4

=item d* => '.'  # Glob Directory.

All options starting with the letter 'd' designate directories in
which to look for files matching the glob specified by option 'g'.
These directories should be in perl's current module search path, else
add to the path using the 'i' option.

=item g => '*_T.pm' # Glob for files to test.

Only files matching this glob will be tested.

=item r => 1  # Search for files recursively.

If set to true, files matching the 'g' glob will be searched for
recursively in all subdirs starting from (and including) those
specified by the 'd' options. FIXME: Currently, it's always true.

=item i* => '' # Directories to add to perl @INC paths.

All options starting with the letter 'i' designate directories that
you want to add to the @INC path for finding modules. They will be
added in the order of the sorted 'i*' keys.

=item t => {} # test() options.

These options will be passed to the test() method, invoked for each
tested file.

=back

=cut

sub files {
  my ($pkg, %options) = @_;
  defined($options{$_}) || ($options{$_} = $_D{f}{$_})
      for qw(g r t);
  $pkg->{options}{c} = defined($options{c}) ? $options{c} : $_D{m}{c};
  $options{d} = $_D{f}{d}
      unless grep substr($_, 0, 1) eq 'd', keys %options;
  my @dirs = map File::Spec->rel2abs($options{$_}),
      grep substr($_, 0, 1) eq 'd', keys %options;
    # Make the options to pass to test() into a string.
  my $t_options = join ', ', map {
    "$_ => '$options{t}{$_}'"
  } keys %{$options{t}};
    # Use the user supplied -i* options and the current contents of
    # @INC as the include path (-I) to the perl we will call.
  my $libs = '';
  $libs = join '',
      map("-I$options{$_} ",
          grep substr($_, 0, 1) eq 'i', sort keys %options),
      map("-I$_ ", @INC);
  my $tot_nb_succ = 0;
  my $tot_nb_fail = 0;
  my $tot_died   = 0;
  my $tot_warned = 0;
  my $tot_hrs  = 0;
  my $tot_mins = 0;
  my $tot_secs = 0;
  my $nb_modules = 0;
  my @found_modules;
  my $wanted = sub {
    my $dir = $File::Find::dir;
    my $file = $_;
    my $spec = "$dir/$file";
    return if -d $spec;
    return unless matches_glob($spec, $options{g});
    my $module = extract_module_name($spec);
    return unless defined $module;
    push @found_modules, $module;
  };
  find($wanted, $_) for @dirs;
  for my $module (sort @found_modules) {
    my (undef, $file_name) = tempfile(UNLINK => 1);
      # Try to make quotes OS-neutral.
    my $prog = qq{$^X $libs -e "use $module; }
        . qq{t()->tee_to(q[$file_name]); test(in_files => 1, $t_options)"};
    system "$prog";
    my $result = read_file($file_name);
      # Parse out the results line.
      # +1 -1 -d -w (00h:00m:00s) Module2_T
    my ($result_line, $nb_succ, $nb_fail, $died_sign, $warned_sign,
        $hrs, $mins, $secs)
        = $result =~ /
          (\s+ \# \s+
          \+(\d+) \s+ -(\d+) \s+
          ([+-])d \s+ ([+-])w \s+
          \( (\d+)h : (\d+)m : (\d+)s \) \s+
          $module
        )/x;
    $tot_nb_succ += $nb_succ || 0;
    $tot_nb_fail += $nb_fail || 0;
    ++$tot_died   if ($died_sign || '') eq '+';
    ++$tot_warned if ($warned_sign || '') eq '+';
    $tot_hrs  += $hrs || 0;
    $tot_mins += $mins || 0;
    $tot_secs += $secs || 0;
    ++$nb_modules;
  };
  my $tot_time = _elapsed_str($tot_hrs * 3600 + $tot_mins * 60 + $tot_secs);
  $pkg->printk('summary',
    sprintf"  # Total +%s -%d %dd %dw ($tot_time) in %d modules.\n",
    $tot_nb_succ,
    $tot_nb_fail,
    $tot_died,
    $tot_warned,
    $nb_modules,
  );
}

# --------------------------------------------------------------------
sub glob_to_regex {
  my $glob = shift;
  $glob =~ s/\./\\./g;
  $glob =~ s/\*/\.*/g;
  $glob =~ s/\?/./g;
      # Insert anchors for start and end of string.
  $glob =~ s/^/\^/g;
  $glob =~ s/$/\$/g;
  return $glob;
}

# --------------------------------------------------------------------
sub _adjust_verbosity {
  my $val = shift;
  return ($val =~ /^(0|1|2)$/) ? $val : REPORT_FAILURES;
}

# --------------------------------------------------------------------

=begin maint $t->printk ($color_tag, $text)

$color_tag, which will map into the color table, is one of:

  what
  died
  warning
  summary
  success
  failure
  diag
  default

=end maint

=cut

sub printk {
  my ($self, $color_tag, $text) = @_;
  print($text), return unless $gColor->{id} && $self->{options}{c};
  my $raw_color = $_D{c}{$color_tag} || $_D{c}{default};
  my $cooked_color = $gColor->{palette}{$raw_color};
  if ($gColor->{id} eq 'Win32Console') {
    $gColor->{out}->Attr($cooked_color);
    $gColor->{out}->Write($&) while $text =~ /.{1,1000}/gs;
    $gColor->{out}->Attr($::FG_GRAY | $::BG_BLACK);
  }
  elsif ($gColor->{id} eq 'ANSI') {
    print Term::ANSIColor::color($cooked_color);
      # Make sure the color reset command is part of the last line
      # (simplifies testing).
    my $chomped = chomp $text;
    print $text;
    print Term::ANSIColor::color('reset');
    print "\n" if $chomped;
  }
    # Unknown color id.
  else {
    print $text;
  }
}

# --------------------------------------------------------------------

=head2 $t->reset_options ()

Resets all options to their default values. To display them, run
, as defined in %_D.

=cut

sub reset_options {
  for my $what (qw(t f m c)) {
    $t->{options}{$_} = $_D{$what}{$_} for keys %{$_D{$what}};
  }
}

# --------------------------------------------------------------------

=head2 $t->options ()

Returns a ref to the hash representing current option settings. Its
structure is:

=cut

sub options { $t->{options} }

# --------------------------------------------------------------------

=head2 $t->time_took ()

Returns the number of seconds the last test() run took. This value is
rough and is of limited use for benchmarking.

=cut

sub time_took { $_[0]->{time_took} }

# --------------------------------------------------------------------

=head2 $t->nb_fail ()

Returns the number of tests that failed during the last test() run.

=cut

sub nb_fail { $_[0]->{nb_fail} }

# --------------------------------------------------------------------

=head2 $t->nb_succ ()

Returns the number of tests that succeeded during the last test() run.

=cut

sub nb_succ { $_[0]->{nb_succ} }

# --------------------------------------------------------------------

=head2 $t->died ()

Returns the number of test()s that die`d.

=cut

sub died { $_[0]->{died} }

# --------------------------------------------------------------------

=head2 $t->warned ()

Returns the number of test()s that had warnings.

=cut

sub warned { $_[0]->{warned} }

# --------------------------------------------------------------------
sub _incr_nb_fail  { ++$_[0]->{nb_fail} }

sub _incr_nb_succ  { ++$_[0]->{nb_succ} }

# --------------------------------------------------------------------

=begin maint $pkg::_elapsed_str ($seconds)

Returns a string like '00:00:05' representing a duration of $seconds
as a 'hours:minutes:seconds' equivalent.

=end maint

=cut

sub _elapsed_str {
  my $seconds = shift;
  my $hr = int($seconds / 3600);
  $seconds -= $hr * 3600;
  my $mi = int($seconds / 60);
  my $se = $seconds - $mi * 60;
  sprintf "%02dh:%02dm:%02ds", $hr, $mi, $se;
}

# --------------------------------------------------------------------
sub extract_module_name {
  my $spec = shift;
    # Extract the module name from the file.
  my $contents = read_file($spec);
  my ($module) = $contents =~ /^\s*package\s+(\S+);/m;
  return $module;
}

# --------------------------------------------------------------------
sub matches_glob {
  my ($file_spec, $glob) = @_;
    # Strip leading '^' of resulting regex.
  my $regex = substr(glob_to_regex($glob), 1);
  return $file_spec =~ /$regex/;
}

# --------------------------------------------------------------------
package Test::Usage::Example;

sub new {
  my ($pkg, $label, $sub_ref) = @_;
  my $self = bless {}, $pkg;
  @{$self}{qw(label sub_ref)} = ($label, $sub_ref);
  return $self;
}

# --------------------------------------------------------------------
1;
__END__

=head1 MOTIVATION

Back in the late 90s, readings on Extreme Programming prompted me to
try out what is now called Test Driven Development (TDD). The idea is
that before you write code, you write a test that demonstrates what
the code should do.  You can find on the web a lot of literature on
TDD, and all I'll say here is that it has radically changed my
development experience.

So at the time, I started building a Perl tool that would help our
team at work to benefit from TDD.  The Test::Usage module is the
current incarnation of this tool.

One feature of the module is that the user can ask to run only one or
a selected number of tests at a time; during code development, we
didn't always care to run the whole test suite each time a change was
made.

It turned out that the test files, containing all those examples(),
required the same intensity of work and refactoring as the main code
itself. The embodied examples evolved as understanding of the
requirements did. I think it helped us build better software.

=head1 Using Test::Usage in a standard Perl module distribution.

The supplied ./share/TestUsageSample is an example Perl module that
maintains its test suites with Test::Usage.

If you want to distribute your module in the standard Perl fashion,
and want to make your Test::Usage tests the ones to be run, you need
to make Test::Usage a prerequisite in your Makefile.PL, and you can
use a test.pl file whose contents are like this:

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

(Note that this will be evaluated in the 'main' package, where files()
is visible.)

=head1 BUGS

No checks are made for duplicated labels.

We want our test module to have as little influence as possible on
what is being tested, but this can be problematic.  For example,
suppose the module being tested needs package Foo, but forgot to 'use'
it. If our testing module uses Foo, the test will not reveal its
absence from the main program.

If a module we are testing has an END block, it won't be invoked in
time for testing.

=head1 AUTHOR

Luc St-Louis, E<lt>lucs@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 by Luc St-Louis

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.3 or,
at your option, any later version of Perl 5 you may have available.

=cut

