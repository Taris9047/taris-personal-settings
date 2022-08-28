# ROCm Installation on Pop! OS 22.04
ROCm is CUDA of AMD and is catching up the scene vigorously according to some news outlets. Also, I prefer AMD GPUs instead of NVIDIA's due to their default color schemes. So, why not set up ROCm instead of closed source NVIDIA? AMD even provides their newest drivers through Linux kernels even. My 6700 XT works out of box without any problem on Linux unlike NVIDIA.

However, there are a few caveats to turn around to install ROCm on this Pop! OS correctly. Let's do this.

## Download Drivers
At first, we need to download the driver, but we will not actually install kernel DKMS module since the drivers are already shipped with Linux kernels. However, since we need to use ROCm, we need to download the driver installation package from AMD. Here is the [https://www.amd.com/en/support/linux-drivers](link). Download one for Ubuntu 22.04 or 22.04.01. 

## Installation
Ok, if we downloaded the deb file, install it. It will install a driver installation script into the system. Say, `/usr/bin/amdgpu-install` This is just a bash script that will add package information into repository database. Since Pop! OS is based on Ubuntu, most of those information is compatible. However, a few caveats exists since the package management system has a bit of deviation from Ubuntu. Now the list of fixes...

### Editing `amdgpu-install`
Now we need to do some surgery work. At first, we need to edit `amdgpu-install` to avoid some pesky error messages which comes from the differences between Pop! OS and Ubuntu. 
```bash
sudo vi /usr/bin/amdgpu-install
```
Then you will find `function os_release()` and search for something like this:
```bash
case "$ID" in
ubuntu|linuxmint|debian)
  PKGUPDATE="apt-get update"
  ...
```
Add `pop` to the case directive. It will look like below after edit.
```
case "$ID" in
ubuntu|linuxmint|debian|pop)
  PKGUPDATE=...
```
As you can see here, they even support Linux Mint!!

Now, we need to remove checking on `kernel-modules-extra` packages. Scroll down to the function: `function debian_build_package_list()` and comment out `if` directive on `linux-modules-extra`. It will look like this after  making edits:
```bash
function debian_build_package_list() {
	if [[ ! "${OPTIONS[*]}" =~ "no-dkms" ]]; then
		if apt-cache show linux-headers-$(uname -r) &>/dev/null; then
			PACKAGES=(${PACKAGES[*]} linux-headers-$(uname -r))
		fi
		#if apt-cache show linux-modules-extra-$(uname -r) &>/dev/null
		#then
		#	PACKAGES=(${PACKAGES[*]} linux-modules-extra-$(uname -r))
		#fi
	fi
}
```
Once done, save it and head to next step.

### rocm-llvm repackaging.
Not sure why, but some of amd's packages are not very friendly even to Ubuntu 22.04. Huh? Whaaaat? That one is `rocm-llvm`. If we do not massage this package, `amdgpu-install` will complain `rocm-llvm` cannot be installed due to package dependency mismatches. Therefore, we need to install `rocm-llvm` manually from massaged package. This section can be found on the [Ref. 2][2].

1. Download `rocm-llvm` deb file...
```apt download rocm-llvm```

2. Extract the deb file
```ar x rocm-llvm_14.0.0.22204.50200-65_amd64.deb```

3. Now we need to extract `control.tar.xz` from the extracted deb.
```tar xf control.tar.xz```

4. Then we get a file `control` which need to be edited. Open it to find `Depends:` section to be edited look like below.
```
Depends: python3, libc6, libstdc++6|libstdc++8, libstdc++-5-dev|libstdc++-7-dev|libstdc++-10-dev, libgcc-5-dev|libgcc-7-dev|libgcc-10-dev, rocm-core
```
Note that we are changing three sections:
```python --> python3```
```libstdc++-5-dev|libstdc++-7-dev --> libstdc++-5-dev|libstdc++-7-dev|libstdc++-10-dev```
```libgcc-5-dev|libgcc-7-dev --> libgcc-5-dev|libgcc-7-dev|libgcc-10-dev```

Then save the file to make the deb package.

```bash
tar c postinst prerm control | xz -c > control.tar.xz
ar rcs rocm-llvm_14.0.0.22204.50200-65_amd64.deb debian-binary control.tar.xz data.tar.xz
```
Note that the rocm-llvm_xxxx might differ in later time. But make sure the orders of those `tar.xz` files are listed correctly.

Now, then install those `-10-dev` stuffs.
```bash
sudo apt install libstdc++-10-dev libstdc-10-dev
```

Also, manage some group privilege stuffs for current user.
```bash
sudo usermod -aG render $LOGNAME
sudo usermod -aG video $LOGNAME
```

### Finally, Installing the ROCm packages!!
At first, we need to install `rocm-core` to install the repackaged `rocm-llvm`.
```bash
sudo apt install rocm-core
```
Then, let's actually install the repackaged `rocm-llvm`.
```bash
sudo dpkg -i rocm-llvm_14.0.0.22204.50200-65_amd64.deb
```
A lot of packages will install.

Finally, let's call the `amdgpu-install` script.
```bash
amdgpu-install --no-dkms --no-32 --usecase=rocm
```
The reasoning of `--no-dkms` is because all the linux Kernels ship with AMD GPU drivers. And we do not obviously need 32 bit libraries. Lastly, we are installing `ROCm` with `--usecase=rocm`.

## Conclusion
That's all. Due to lack of pre-packaged solutions unlike NVIDIA's CUDA, we had to work on some bash Fu's. But this is worth it since we don't need to update ROCm packages every time the graphics driver updates. Also, recent ROCm actually supports all those AMD's newest RDNA2 GPUs. Now, let's get back to work!



## References
[1]. https://www.amd.com/en/support/linux-drivers
[2]. https://gist.github.com/FCLC/8c1f4d28d65a2e6d40b82f82c8fe4e08
[3]. https://github.com/RadeonOpenCompute/ROCm/issues/1713
