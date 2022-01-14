Name:          phatsort-haskell
Version:       {{VERSION}}
Release:       1%{?dist}
Summary:       FAT filesystem sort utility
License:       MIT
URL:           https://github.com/ExtremaIS/phatsort-haskell
Source0:       phatsort-haskell-{{VERSION}}.tar.xz
BuildArch:     {{ARCH}}
BuildRequires: make
Requires:      glibc,gmp
#ExcludeArch:

%description
PhatSort is a utility that sorts files and directories on a FAT filesystem by
creating new directories and moving ("renaming") the files in the desired
order, while the filesystem is mounted.

%global debug_package %{nil}

%prep
%setup -q

%build

%install
make install DESTDIR=%{buildroot} PREFIX=/usr

%check
make test

%files
%{_bindir}/phatsort
%{_bindir}/seqcp
%{_mandir}/man1/phatsort.1.gz
%{_mandir}/man1/seqcp.1.gz
%{_datadir}/doc/%{name}/

%changelog
* {{DATE}} {{RPMFULLNAME}} <{{RPMEMAIL}}> - {{VERSION}}-1
- Release {{VERSION}}
