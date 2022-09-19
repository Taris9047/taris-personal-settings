## Some Notes
### Fedora - Deprecated now
1. Perl module FindBin is missing.
2. Fedora might probably missing perl package entirely.
3. Try to install FindBin by calling perl package manager: cpan.
4. Then install all the packages to run cpan.
5. After running cpan, type install FindBin

### XVIDCORE link does not work!!
Alternatively, you can use:
https://fossies.org/linux/misc/xvidcore-1.3.7.tar.gz

### OpenCL
If the system does not have CUDA and has OpenCL (detects `clinfo` command), it will automatically add `--enable-opencl` into FFMpeg's configure. In that case, you need to install a few packages to compile successfully. 

For Ubuntu:
ocl-icd-opencl-dev
opencl-headers


