BEGIN{
FS="    "
}

{
time=$1
rain=$2

if(lastline!=$0)
print $0

lastline=$0


}
