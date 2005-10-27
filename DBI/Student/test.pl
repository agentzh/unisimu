my @files = map glob, @ARGV;
  use SQL::Translator;
  my $translator     = SQL::Translator->new(
      from           => 'MySQL',
      filename       => [@files],
      to             => 'TTSchema',
      producer_args  => {
          ttfile     => 'insert.sql.tt.tt',  # Template file to use

          # Extra template variables
          tt_args     => {
              author => "Agent Zhang",
          },

          # Template config options
          tt_args     => {
              INCLUDE_PATH => '/foo/templates',
          },
      },
  );
  print $translator->translate;
