test:
	set HARNESS_OK_SLOW = 1
		perl -MExtUtils::Command::MM \
		-e "@ARGV = map glob, @ARGV; test_harness(0, '.', '.');" t\..\*.t