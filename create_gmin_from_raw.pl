#!/usr/bin/perl
MAIN:
{
	$nargs = 4-1;
	if ($#ARGV != $nargs) {
        print STDERR "Usage:\n $0 DIRECTORY SITEID YEAR VERBOSE\n";
		print STDERR "DIRECTORY above the data/ directory\n";
		print STDERR "SITEID is arbitrary site identificatio\n";
		print STDERR "YEAR beginning year of data\n";
		print STDERR "VERBOSE is the verbose flag.\n";
        exit;
	}
#
# ### Set defaults
#
	$verbose  = "0";			# Verbose logging to STDOUT
	$net_id   = "???";			# Network ID
#
# ### Assign command line parameter values
#
	$dir      = shift;
	$net_id   = shift;
	$year     = shift;
	$verbose  = shift;
#
# ### Print to screen
#
	if($verbose == 1) {
		printf("NETWORK: %s\n", $net_id);
		printf("YEAR:    %d\n", $year);
	}
#
# ### Execute $prog using $prog.i in current directory.
#
	print("***********************************************************\n");
	printf("Executing preproc...\n");
	print("***********************************************************\n");
	$command = "preproc.pl $dir $net_id $year $verbose";
	print("$command \n\n");
	system($command);

	print("***********************************************************\n");
	printf("Executing preproc.pl...\n");
	print("***********************************************************\n");
	$command = "rgmin.pl $dir $net_id $verbose";
	print("$command \n\n");
	system($command);

	print("Finished! \n");
}
