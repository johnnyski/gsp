#!/usr/bin/perl
MAIN:
{
	$nargs = 3;
	if ($#ARGV != $nargs-1) {
        print STDERR "Usage:\n $0 DIRECTORY SITEID VERBOSE\n";
		print STDERR "DIRECTORY above the data/ directory\n";
		print STDERR "SITEID is arbitrary site identificatio\n";
		print STDERR "VERBOSE is the verbose flag.\n";
        exit;
	}
#
# ## Set defaults
#
	$prog     = "rgmin";		# Program name
	$verbose  = "0";			# Verbose = on
	$net_id   = "???";			# Network ID
#	$beg_year = "0";			# IBYEAR
#
# ## Assign command line parameter values
#
	$dir      = shift;
	$net_id   = shift;
	$verbose  = shift;

	$in_dir     = $dir."/data/gag/".$net_id."/";    # Input directory
	$out_dir    = $dir."/data/gmin/";	            # Output directory
	$loc_file   = $dir."/data/sitelist/".$net_id."_loc.dat"; # Site list file
	if ($net_id eq "KSO"){
		$in_dir     = $dir."/data/gag/KSC/";    # Input directory
		$loc_file   = $dir."/data/sitelist/KSC_loc.dat";
	}

	$radar_file = $dir."/data/sitelist/"."radar.dat";        # Site list file

	$directive = $prog.".i";
#
# ### Print to screen
#
	
	print $prog."\n";
	print $verbose."\n";
	print $net_id."\n";
	print $beg_year."\n";
	print $in_dir."\n";
	print $out_dir."\n";
	print $loc_file."\n";
 	print $radar_file."\n";
#
# ### Open $prog.i file and write
#
  	open(FILE,"> $directive");
	print FILE $verbose."\n";
	print FILE $net_id."\n";
	print FILE $in_dir."\n";
	print FILE $out_dir."\n";
	print FILE $loc_file."\n";
	print FILE $radar_file."\n";


	
# ### Get a list of all of the input (*.gag) files, write their
# ### names one per line in the $prog.i file.
#
	open(LIST,"ls $in_dir  |"); # Create list of input filenames
	$i = 0;
	while(<LIST>) {		# For each filename in LIST, process...
		$i++;
		chop;
		$gag_file  = $_;	# Gag file
		print FILE $gag_file."\n";
		print $gag_file."\n";
	}
	print "Found $i files\n";
#
# ### Close the $prog.i file
#
	close(FILE);

	
##       my addition ----
	print STDERR "==== $net_id .... $in_dir\n";
	if ($net_id eq "STJ"){
		die "list1 failed" if(! open(LIST1,"ls -C1 $in_dir |"));
	open(LIST2,"ls -l $in_dir  |"); # Create list of input filenames
	$i = 0;
	while(<LIST2>) {		# For each filename in LIST2, process...
		$i++;
		($perm,$f2,$own,$grp,$fsize,$cmon,$cday,$ctime,$ifile) = split(' ',$_);
		chop;
		if($perm ne "total") {
		$gag2_file  = $in_dir.$ifile ;	# Gag file
#  		printf("%s\n",$gag2_file);
		if($fsize == 0){
			$rem_command = "rm ".$gag2_file;
			system($rem_command); # remove empty files

		} else{
		   $sort_command = "awk -f sort1.awk ".$gag2_file." > ".$gag2_file."1";
#			printf("%s\n",$gag2_file."1");
			system($sort_command); # sort records in time sequence
			$move_command = "mv ".$gag2_file."1"." ".$gag2_file;
			system($move_command);

			$delrep_command = "awk -f sort2.awk ".$gag2_file." > ".$gag2_file."1";
			system($delrep_command);# delete repeatting records
			$move_command = "mv ".$gag2_file."1"." ".$gag2_file;
			system($move_command);

			$removal_command = "awk -f remove.awk ".$gag2_file." > ".$gag2_file."1";
			system($removal_command);# remove data for current yea-1/+1
			$move_command = "mv ".$gag2_file."1"." ".$gag2_file;
			system($move_command);

	      }
		}
#		print "filesize: $fsize\n";
#		print $gag_file."\n";
	}
	print "Haaaaa,  Found $i files\n";
		close(LIST2);

  }
#
#
# ### Execute $prog using $prog.i in current directory.
#
	system($prog);
}



