    use strict;
	use warnings;
	use GD::Animation;
	use Data::Dumper;

	my %c;
	$c{white} = [255, 255, 255];
	$c{red} = [255, 0, 0];
	for(0..100){
		$c{'c'.$_} = [int rand 256, int rand 256, int rand 256];
	}


    my $anim = GD::Animation->new(
        800, 800,
        loops => 0,
        colors => \%c,	
			
       
    );
	
	#warn Data::Dumper::Dumper(%c);
   
   
    my $x = 200;
	my $y = 200;
	for(1..6) {
		my $img = $anim->add_frame();
		
		my %colors = $anim->color_map;
		$img->bgcolor( $colors{white} );
        $img->fgcolor( $colors{red}   );  
		my $poly = GD::Polygon->new;
		$poly->addPt($x+130, $y+16);
		$poly->addPt($x+8, $y+232);
		$poly->addPt($x+208, $y+232);
		$poly->addPt($x+181, $y+190);
		$poly->addPt($x+83, $y+189);
		$poly->addPt($x+181, $y+16);
		$img->filledPolygon($poly, $colors{'c'.(int rand 100)});

		my $poly2 = GD::Polygon->new;
		$poly2->addPt($x+181, $y+16);
		$poly2->addPt($x+83, $y+189);
		$poly2->addPt($x+132, $y+189);
		$poly2->addPt($x+182, $y+104);
		$poly2->addPt($x+281, $y+275);
		$poly2->addPt($x+307, $y+232);
		$img->filledPolygon($poly2, $colors{'c'.(int rand 100)});

		my $poly3 = GD::Polygon->new;
		$poly3->addPt($x+157, $y+146);
		$poly3->addPt($x+182, $y+104);
		$poly3->addPt($x+281, $y+275);
		$poly3->addPt($x+30, $y+275);
		$poly3->addPt($x+8, $y+231);
		$poly3->addPt($x+208, $y+232);
		$img->filledPolygon($poly3, $colors{'c'.(int rand 100)});

	  
		#$red = $im->colorAllocate(rand()*255, rand()*255, rand()*255);
		
		
	}
   

    binmode \*STDOUT;
    print $anim->gif_data;
