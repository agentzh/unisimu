use GD::Simple;    # create a new image

binmode \*STDOUT;

    $img = GD::Simple->new(400,250);    # draw a red rectangle with blue borders
    $img->bgcolor('red');
    $img->fgcolor('blue');
    $img->rectangle(10,10,50,50);    # draw an empty rectangle with green borders
    $img->bgcolor(undef);
    $img->fgcolor('green');
    $img->rectangle(30,30,100,100);    # move to (80,80) and draw a green line to (100,190)
    $img->moveTo(80,80);
    $img->lineTo(100,190);    # draw a solid orange ellipse
    $img->moveTo(110,100);
    $img->bgcolor('orange');
    $img->fgcolor('orange');
    $img->ellipse(40,40);    # draw a black filled arc
    $img->moveTo(150,150);
    $img->fgcolor('black');
    $img->arc(50,50,0,100);    # draw a string at (10,180) using the default
    # built-in font
    $img->moveTo(10,180);
    $img->string('This is very simple');    # draw a string at (280,210) using 20 point
    # times italic, angled upward 90 degrees
    $img->moveTo(280,210);
    $img->font('Times:italic');
    $img->fontsize(20);
    $img->angle(-90);
    $img->string('This is very fancy');    # some turtle graphics
    $img->moveTo(300,100);
    $img->penSize(3,3);
    $img->angle(0);
    $img->line(20);   # 20 pixels going to the right
    $img->turn(30);   # set turning angle to 30 degrees
    $img->line(20);   # 20 pixel line
    $img->line(20);
    $img->line(20);
    $img->turn(-90); # set turning angle to -90 degrees
    $img->line(50);  # 50 pixel line    # draw a cyan polygon edged in blue
    my $poly = new GD::Polygon;
    $poly->addPt(150,100);
    $poly->addPt(199,199);
    $poly->addPt(100,199);
    $img->bgcolor('cyan');
    $img->fgcolor('blue');
    $img->penSize(1,1);
    $img->polygon($poly);   # convert into png data
   print $img->png;
