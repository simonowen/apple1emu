#!/usr/bin/perl -w
#
# Convert PNG image to raw font binary for Apple 1 emulator
#
# Makes lots of assumptions about the input image!

use Compress::Zlib;

# Input image and output data file
my $input = 'font.png';
my $output = 'font.bin';

# Our characters are 6 pixels wide
$chrw = 6;


# Slurp the entire PNG image
open INPUT, "<$input" and binmode INPUT or die "$input: $!\n";
read INPUT, $data='', -s $input;
close INPUT;

# Find and extract the image dimensions
$data =~ /IHDR(.{8})/s;
($w,$h) = unpack "N2", $1;

# Extract and expand the compressed image data
($data) = $data =~ /IDAT(.*).{8}IEND/s;
$data = Compress::Zlib::uncompress($data);

# Remove the type byte from the start of each line
$w_2 = $w/2;
$data =~ s/.(.{$w_2})/$1/sg;

@data = ();

# Unpack the pixel nibbles
foreach (unpack "C*", $data) {
  push @data, $_>>4, $_&1;
}


open OUTPUT, ">$output" and binmode OUTPUT or die "$output: $!\n";

# Process all characters
foreach $chr (0..$w/$chrw-1)
{
  # Process the image line by line
  foreach $y (0..$h-1)
  {
    # Locate the pixel data for the current character
    my $x = $chr*$chrw + $w*$y;
    my $b = 0;

    # Pack the 1bpp data into a single byte
    foreach (@data[$x..$x+$chrw-1]) {
      $b = ($b << 1) | $_;
    }

    # Left-align within the byte and output it
    print OUTPUT chr($b << (8-$chrw));
  }
}

close OUTPUT;
