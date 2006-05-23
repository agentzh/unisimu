    use strict;
	use warnings;
	use GD::Animation;

    my $anim = GD::Animation->new(
        800, 800,
        loops => 0,
        colors => {
            white => [255, 255, 255],
            red   => [255, 0, 0],
			c1	=> [255, 255, 0],
			c2 => [255, 0, 255],
			c3 => [0, 255, 255],
        },
    );
   
    my $x = 50;
    my @files = glob("m/*.png");
	for(@files) {
		my $im = new GD::Image->new($_);
		warn $_, "\n";
	  
		$anim->add_frame($im);#$red = $im->colorAllocate(rand()*255, rand()*255, rand()*255);
		
		
	}
   

    binmode \*STDOUT;
    print $anim->gif_data;
