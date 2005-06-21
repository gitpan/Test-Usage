use 5.008;
use ExtUtils::MakeMaker;

WriteMakefile (
  NAME          => 'Test::Usage',
  VERSION_FROM  => 'lib/Test/Usage.pm',
  dist => {
      # This will work only if the README file already exists, so
      # 'touch' it if necessary.
    PREOP => 'pod2text lib/Test/Usage.pm > README',
  },
  PREREQ_PM => {
    IO::Capture::Stderr => 0,
    IO::Capture::Stdout => 0,
    File::Slurp => 0,
  },
  ABSTRACT_FROM  => 'lib/Test/Usage.pm',
  AUTHOR         => 'Luc St-Louis <lucs@cpan.org>',
);

