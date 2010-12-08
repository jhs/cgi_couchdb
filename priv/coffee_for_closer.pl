#!/usr/bin/env perl
#
# This is stupid. Erlang ports have no way to close the writer handle
# and then read the reader handle. That would be useful to e.g. send
# source code to a dynamic language and read what output comes back.
# This tool opens the actual CGI program as a subprocess. Then it receives
# from Erlang a "NNNN\n" preamble indicating the total data length. When
# that much data has been forwarded to CGI, it closes the filehandle so
# the subprocess can send results back to Erlang.

use strict;

open LOG, '>', '/tmp/log' or die $!;
print LOG "Starting closer: $$: @ARGV\n";

my $length = <STDIN>;
chomp $length;
die "Unknown length: $length" unless($length =~ /^(\d+)$/);
print LOG "Reading $length bytes from STDIN\n";

open CGI, '|-', 'ruby' or die $!;

my ($buf, $got_size);
my $CHUNK_SIZE = 4096;
my $remaining = $length;

while($remaining > 0) {
  $got_size = read(STDIN, $buf, ($remaining < $CHUNK_SIZE) ? $remaining : $CHUNK_SIZE);
  print LOG "Received $got_size from STDIN\n";
  die "Received end-of-file, but $remaining bytes (out of total $length) expected" if($got_size == 0);
  $remaining -= $got_size;
  die "Received greater-than-expected data ($remaining, expected=$length)" if($remaining < 0);

  print CGI $buf;
}

print LOG "Remaining is just right: $remaining\n";
close CGI;

#print STDOUT "Content-Type: text/html\r\n\r\n";
#print STDOUT "<a href='/blah'>Link!</a><br>\n";
#print STDOUT "CGI: Thank you, I got it all\n";

print LOG "Exiting\n";
close LOG;

exit(0);
