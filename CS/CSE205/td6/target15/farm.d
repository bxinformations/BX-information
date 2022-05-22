
rtarget:     file format elf64-x86-64


Disassembly of section .init:

0000000000400bc8 <_init>:
  400bc8:	48 83 ec 08          	sub    $0x8,%rsp
  400bcc:	48 8b 05 25 54 20 00 	mov    0x205425(%rip),%rax        # 605ff8 <__gmon_start__>
  400bd3:	48 85 c0             	test   %rax,%rax
  400bd6:	74 05                	je     400bdd <_init+0x15>
  400bd8:	e8 33 01 00 00       	callq  400d10 <__gmon_start__@plt>
  400bdd:	48 83 c4 08          	add    $0x8,%rsp
  400be1:	c3                   	retq   

Disassembly of section .plt:

0000000000400bf0 <.plt>:
  400bf0:	ff 35 12 54 20 00    	pushq  0x205412(%rip)        # 606008 <_GLOBAL_OFFSET_TABLE_+0x8>
  400bf6:	ff 25 14 54 20 00    	jmpq   *0x205414(%rip)        # 606010 <_GLOBAL_OFFSET_TABLE_+0x10>
  400bfc:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000400c00 <strcasecmp@plt>:
  400c00:	ff 25 12 54 20 00    	jmpq   *0x205412(%rip)        # 606018 <strcasecmp@GLIBC_2.2.5>
  400c06:	68 00 00 00 00       	pushq  $0x0
  400c0b:	e9 e0 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400c10 <__errno_location@plt>:
  400c10:	ff 25 0a 54 20 00    	jmpq   *0x20540a(%rip)        # 606020 <__errno_location@GLIBC_2.2.5>
  400c16:	68 01 00 00 00       	pushq  $0x1
  400c1b:	e9 d0 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400c20 <srandom@plt>:
  400c20:	ff 25 02 54 20 00    	jmpq   *0x205402(%rip)        # 606028 <srandom@GLIBC_2.2.5>
  400c26:	68 02 00 00 00       	pushq  $0x2
  400c2b:	e9 c0 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400c30 <strncmp@plt>:
  400c30:	ff 25 fa 53 20 00    	jmpq   *0x2053fa(%rip)        # 606030 <strncmp@GLIBC_2.2.5>
  400c36:	68 03 00 00 00       	pushq  $0x3
  400c3b:	e9 b0 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400c40 <strcpy@plt>:
  400c40:	ff 25 f2 53 20 00    	jmpq   *0x2053f2(%rip)        # 606038 <strcpy@GLIBC_2.2.5>
  400c46:	68 04 00 00 00       	pushq  $0x4
  400c4b:	e9 a0 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400c50 <puts@plt>:
  400c50:	ff 25 ea 53 20 00    	jmpq   *0x2053ea(%rip)        # 606040 <puts@GLIBC_2.2.5>
  400c56:	68 05 00 00 00       	pushq  $0x5
  400c5b:	e9 90 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400c60 <write@plt>:
  400c60:	ff 25 e2 53 20 00    	jmpq   *0x2053e2(%rip)        # 606048 <write@GLIBC_2.2.5>
  400c66:	68 06 00 00 00       	pushq  $0x6
  400c6b:	e9 80 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400c70 <mmap@plt>:
  400c70:	ff 25 da 53 20 00    	jmpq   *0x2053da(%rip)        # 606050 <mmap@GLIBC_2.2.5>
  400c76:	68 07 00 00 00       	pushq  $0x7
  400c7b:	e9 70 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400c80 <printf@plt>:
  400c80:	ff 25 d2 53 20 00    	jmpq   *0x2053d2(%rip)        # 606058 <printf@GLIBC_2.2.5>
  400c86:	68 08 00 00 00       	pushq  $0x8
  400c8b:	e9 60 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400c90 <memset@plt>:
  400c90:	ff 25 ca 53 20 00    	jmpq   *0x2053ca(%rip)        # 606060 <memset@GLIBC_2.2.5>
  400c96:	68 09 00 00 00       	pushq  $0x9
  400c9b:	e9 50 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400ca0 <alarm@plt>:
  400ca0:	ff 25 c2 53 20 00    	jmpq   *0x2053c2(%rip)        # 606068 <alarm@GLIBC_2.2.5>
  400ca6:	68 0a 00 00 00       	pushq  $0xa
  400cab:	e9 40 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400cb0 <close@plt>:
  400cb0:	ff 25 ba 53 20 00    	jmpq   *0x2053ba(%rip)        # 606070 <close@GLIBC_2.2.5>
  400cb6:	68 0b 00 00 00       	pushq  $0xb
  400cbb:	e9 30 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400cc0 <read@plt>:
  400cc0:	ff 25 b2 53 20 00    	jmpq   *0x2053b2(%rip)        # 606078 <read@GLIBC_2.2.5>
  400cc6:	68 0c 00 00 00       	pushq  $0xc
  400ccb:	e9 20 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400cd0 <__libc_start_main@plt>:
  400cd0:	ff 25 aa 53 20 00    	jmpq   *0x2053aa(%rip)        # 606080 <__libc_start_main@GLIBC_2.2.5>
  400cd6:	68 0d 00 00 00       	pushq  $0xd
  400cdb:	e9 10 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400ce0 <signal@plt>:
  400ce0:	ff 25 a2 53 20 00    	jmpq   *0x2053a2(%rip)        # 606088 <signal@GLIBC_2.2.5>
  400ce6:	68 0e 00 00 00       	pushq  $0xe
  400ceb:	e9 00 ff ff ff       	jmpq   400bf0 <.plt>

0000000000400cf0 <gethostbyname@plt>:
  400cf0:	ff 25 9a 53 20 00    	jmpq   *0x20539a(%rip)        # 606090 <gethostbyname@GLIBC_2.2.5>
  400cf6:	68 0f 00 00 00       	pushq  $0xf
  400cfb:	e9 f0 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400d00 <fprintf@plt>:
  400d00:	ff 25 92 53 20 00    	jmpq   *0x205392(%rip)        # 606098 <fprintf@GLIBC_2.2.5>
  400d06:	68 10 00 00 00       	pushq  $0x10
  400d0b:	e9 e0 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400d10 <__gmon_start__@plt>:
  400d10:	ff 25 8a 53 20 00    	jmpq   *0x20538a(%rip)        # 6060a0 <__gmon_start__>
  400d16:	68 11 00 00 00       	pushq  $0x11
  400d1b:	e9 d0 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400d20 <strtol@plt>:
  400d20:	ff 25 82 53 20 00    	jmpq   *0x205382(%rip)        # 6060a8 <strtol@GLIBC_2.2.5>
  400d26:	68 12 00 00 00       	pushq  $0x12
  400d2b:	e9 c0 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400d30 <memcpy@plt>:
  400d30:	ff 25 7a 53 20 00    	jmpq   *0x20537a(%rip)        # 6060b0 <memcpy@GLIBC_2.14>
  400d36:	68 13 00 00 00       	pushq  $0x13
  400d3b:	e9 b0 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400d40 <time@plt>:
  400d40:	ff 25 72 53 20 00    	jmpq   *0x205372(%rip)        # 6060b8 <time@GLIBC_2.2.5>
  400d46:	68 14 00 00 00       	pushq  $0x14
  400d4b:	e9 a0 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400d50 <random@plt>:
  400d50:	ff 25 6a 53 20 00    	jmpq   *0x20536a(%rip)        # 6060c0 <random@GLIBC_2.2.5>
  400d56:	68 15 00 00 00       	pushq  $0x15
  400d5b:	e9 90 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400d60 <_IO_getc@plt>:
  400d60:	ff 25 62 53 20 00    	jmpq   *0x205362(%rip)        # 6060c8 <_IO_getc@GLIBC_2.2.5>
  400d66:	68 16 00 00 00       	pushq  $0x16
  400d6b:	e9 80 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400d70 <__isoc99_sscanf@plt>:
  400d70:	ff 25 5a 53 20 00    	jmpq   *0x20535a(%rip)        # 6060d0 <__isoc99_sscanf@GLIBC_2.7>
  400d76:	68 17 00 00 00       	pushq  $0x17
  400d7b:	e9 70 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400d80 <munmap@plt>:
  400d80:	ff 25 52 53 20 00    	jmpq   *0x205352(%rip)        # 6060d8 <munmap@GLIBC_2.2.5>
  400d86:	68 18 00 00 00       	pushq  $0x18
  400d8b:	e9 60 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400d90 <bcopy@plt>:
  400d90:	ff 25 4a 53 20 00    	jmpq   *0x20534a(%rip)        # 6060e0 <bcopy@GLIBC_2.2.5>
  400d96:	68 19 00 00 00       	pushq  $0x19
  400d9b:	e9 50 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400da0 <fopen@plt>:
  400da0:	ff 25 42 53 20 00    	jmpq   *0x205342(%rip)        # 6060e8 <fopen@GLIBC_2.2.5>
  400da6:	68 1a 00 00 00       	pushq  $0x1a
  400dab:	e9 40 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400db0 <getopt@plt>:
  400db0:	ff 25 3a 53 20 00    	jmpq   *0x20533a(%rip)        # 6060f0 <getopt@GLIBC_2.2.5>
  400db6:	68 1b 00 00 00       	pushq  $0x1b
  400dbb:	e9 30 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400dc0 <strtoul@plt>:
  400dc0:	ff 25 32 53 20 00    	jmpq   *0x205332(%rip)        # 6060f8 <strtoul@GLIBC_2.2.5>
  400dc6:	68 1c 00 00 00       	pushq  $0x1c
  400dcb:	e9 20 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400dd0 <gethostname@plt>:
  400dd0:	ff 25 2a 53 20 00    	jmpq   *0x20532a(%rip)        # 606100 <gethostname@GLIBC_2.2.5>
  400dd6:	68 1d 00 00 00       	pushq  $0x1d
  400ddb:	e9 10 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400de0 <sprintf@plt>:
  400de0:	ff 25 22 53 20 00    	jmpq   *0x205322(%rip)        # 606108 <sprintf@GLIBC_2.2.5>
  400de6:	68 1e 00 00 00       	pushq  $0x1e
  400deb:	e9 00 fe ff ff       	jmpq   400bf0 <.plt>

0000000000400df0 <exit@plt>:
  400df0:	ff 25 1a 53 20 00    	jmpq   *0x20531a(%rip)        # 606110 <exit@GLIBC_2.2.5>
  400df6:	68 1f 00 00 00       	pushq  $0x1f
  400dfb:	e9 f0 fd ff ff       	jmpq   400bf0 <.plt>

0000000000400e00 <connect@plt>:
  400e00:	ff 25 12 53 20 00    	jmpq   *0x205312(%rip)        # 606118 <connect@GLIBC_2.2.5>
  400e06:	68 20 00 00 00       	pushq  $0x20
  400e0b:	e9 e0 fd ff ff       	jmpq   400bf0 <.plt>

0000000000400e10 <socket@plt>:
  400e10:	ff 25 0a 53 20 00    	jmpq   *0x20530a(%rip)        # 606120 <socket@GLIBC_2.2.5>
  400e16:	68 21 00 00 00       	pushq  $0x21
  400e1b:	e9 d0 fd ff ff       	jmpq   400bf0 <.plt>

Disassembly of section .text:

0000000000400e20 <_start>:
  400e20:	31 ed                	xor    %ebp,%ebp
  400e22:	49 89 d1             	mov    %rdx,%r9
  400e25:	5e                   	pop    %rsi
  400e26:	48 89 e2             	mov    %rsp,%rdx
  400e29:	48 83 e4 f0          	and    $0xfffffffffffffff0,%rsp
  400e2d:	50                   	push   %rax
  400e2e:	54                   	push   %rsp
  400e2f:	49 c7 c0 60 2d 40 00 	mov    $0x402d60,%r8
  400e36:	48 c7 c1 f0 2c 40 00 	mov    $0x402cf0,%rcx
  400e3d:	48 c7 c7 e0 10 40 00 	mov    $0x4010e0,%rdi
  400e44:	e8 87 fe ff ff       	callq  400cd0 <__libc_start_main@plt>
  400e49:	f4                   	hlt    
  400e4a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000400e50 <deregister_tm_clones>:
  400e50:	b8 97 69 60 00       	mov    $0x606997,%eax
  400e55:	55                   	push   %rbp
  400e56:	48 2d 90 69 60 00    	sub    $0x606990,%rax
  400e5c:	48 83 f8 0e          	cmp    $0xe,%rax
  400e60:	48 89 e5             	mov    %rsp,%rbp
  400e63:	77 02                	ja     400e67 <deregister_tm_clones+0x17>
  400e65:	5d                   	pop    %rbp
  400e66:	c3                   	retq   
  400e67:	b8 00 00 00 00       	mov    $0x0,%eax
  400e6c:	48 85 c0             	test   %rax,%rax
  400e6f:	74 f4                	je     400e65 <deregister_tm_clones+0x15>
  400e71:	5d                   	pop    %rbp
  400e72:	bf 90 69 60 00       	mov    $0x606990,%edi
  400e77:	ff e0                	jmpq   *%rax
  400e79:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000400e80 <register_tm_clones>:
  400e80:	b8 90 69 60 00       	mov    $0x606990,%eax
  400e85:	55                   	push   %rbp
  400e86:	48 2d 90 69 60 00    	sub    $0x606990,%rax
  400e8c:	48 c1 f8 03          	sar    $0x3,%rax
  400e90:	48 89 e5             	mov    %rsp,%rbp
  400e93:	48 89 c2             	mov    %rax,%rdx
  400e96:	48 c1 ea 3f          	shr    $0x3f,%rdx
  400e9a:	48 01 d0             	add    %rdx,%rax
  400e9d:	48 d1 f8             	sar    %rax
  400ea0:	75 02                	jne    400ea4 <register_tm_clones+0x24>
  400ea2:	5d                   	pop    %rbp
  400ea3:	c3                   	retq   
  400ea4:	ba 00 00 00 00       	mov    $0x0,%edx
  400ea9:	48 85 d2             	test   %rdx,%rdx
  400eac:	74 f4                	je     400ea2 <register_tm_clones+0x22>
  400eae:	5d                   	pop    %rbp
  400eaf:	48 89 c6             	mov    %rax,%rsi
  400eb2:	bf 90 69 60 00       	mov    $0x606990,%edi
  400eb7:	ff e2                	jmpq   *%rdx
  400eb9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000400ec0 <__do_global_dtors_aux>:
  400ec0:	80 3d f1 5a 20 00 00 	cmpb   $0x0,0x205af1(%rip)        # 6069b8 <completed.6355>
  400ec7:	75 11                	jne    400eda <__do_global_dtors_aux+0x1a>
  400ec9:	55                   	push   %rbp
  400eca:	48 89 e5             	mov    %rsp,%rbp
  400ecd:	e8 7e ff ff ff       	callq  400e50 <deregister_tm_clones>
  400ed2:	5d                   	pop    %rbp
  400ed3:	c6 05 de 5a 20 00 01 	movb   $0x1,0x205ade(%rip)        # 6069b8 <completed.6355>
  400eda:	f3 c3                	repz retq 
  400edc:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000400ee0 <frame_dummy>:
  400ee0:	48 83 3d 38 4f 20 00 	cmpq   $0x0,0x204f38(%rip)        # 605e20 <__JCR_END__>
  400ee7:	00 
  400ee8:	74 1e                	je     400f08 <frame_dummy+0x28>
  400eea:	b8 00 00 00 00       	mov    $0x0,%eax
  400eef:	48 85 c0             	test   %rax,%rax
  400ef2:	74 14                	je     400f08 <frame_dummy+0x28>
  400ef4:	55                   	push   %rbp
  400ef5:	bf 20 5e 60 00       	mov    $0x605e20,%edi
  400efa:	48 89 e5             	mov    %rsp,%rbp
  400efd:	ff d0                	callq  *%rax
  400eff:	5d                   	pop    %rbp
  400f00:	e9 7b ff ff ff       	jmpq   400e80 <register_tm_clones>
  400f05:	0f 1f 00             	nopl   (%rax)
  400f08:	e9 73 ff ff ff       	jmpq   400e80 <register_tm_clones>
  400f0d:	0f 1f 00             	nopl   (%rax)

0000000000400f10 <usage>:
  400f10:	48 83 ec 08          	sub    $0x8,%rsp
  400f14:	48 89 fe             	mov    %rdi,%rsi
  400f17:	83 3d ca 5a 20 00 00 	cmpl   $0x0,0x205aca(%rip)        # 6069e8 <is_checker>
  400f1e:	74 39                	je     400f59 <usage+0x49>
  400f20:	bf 80 2d 40 00       	mov    $0x402d80,%edi
  400f25:	b8 00 00 00 00       	mov    $0x0,%eax
  400f2a:	e8 51 fd ff ff       	callq  400c80 <printf@plt>
  400f2f:	bf b8 2d 40 00       	mov    $0x402db8,%edi
  400f34:	e8 17 fd ff ff       	callq  400c50 <puts@plt>
  400f39:	bf 30 2f 40 00       	mov    $0x402f30,%edi
  400f3e:	e8 0d fd ff ff       	callq  400c50 <puts@plt>
  400f43:	bf e0 2d 40 00       	mov    $0x402de0,%edi
  400f48:	e8 03 fd ff ff       	callq  400c50 <puts@plt>
  400f4d:	bf 4a 2f 40 00       	mov    $0x402f4a,%edi
  400f52:	e8 f9 fc ff ff       	callq  400c50 <puts@plt>
  400f57:	eb 2d                	jmp    400f86 <usage+0x76>
  400f59:	bf 66 2f 40 00       	mov    $0x402f66,%edi
  400f5e:	b8 00 00 00 00       	mov    $0x0,%eax
  400f63:	e8 18 fd ff ff       	callq  400c80 <printf@plt>
  400f68:	bf 08 2e 40 00       	mov    $0x402e08,%edi
  400f6d:	e8 de fc ff ff       	callq  400c50 <puts@plt>
  400f72:	bf 30 2e 40 00       	mov    $0x402e30,%edi
  400f77:	e8 d4 fc ff ff       	callq  400c50 <puts@plt>
  400f7c:	bf 84 2f 40 00       	mov    $0x402f84,%edi
  400f81:	e8 ca fc ff ff       	callq  400c50 <puts@plt>
  400f86:	bf 00 00 00 00       	mov    $0x0,%edi
  400f8b:	e8 60 fe ff ff       	callq  400df0 <exit@plt>

0000000000400f90 <initialize_target>:
  400f90:	55                   	push   %rbp
  400f91:	53                   	push   %rbx
  400f92:	48 81 ec 08 21 00 00 	sub    $0x2108,%rsp
  400f99:	89 f5                	mov    %esi,%ebp
  400f9b:	89 3d 37 5a 20 00    	mov    %edi,0x205a37(%rip)        # 6069d8 <check_level>
  400fa1:	8b 3d c1 51 20 00    	mov    0x2051c1(%rip),%edi        # 606168 <target_id>
  400fa7:	e8 17 1d 00 00       	callq  402cc3 <gencookie>
  400fac:	89 05 32 5a 20 00    	mov    %eax,0x205a32(%rip)        # 6069e4 <cookie>
  400fb2:	89 c7                	mov    %eax,%edi
  400fb4:	e8 0a 1d 00 00       	callq  402cc3 <gencookie>
  400fb9:	89 05 21 5a 20 00    	mov    %eax,0x205a21(%rip)        # 6069e0 <authkey>
  400fbf:	8b 05 a3 51 20 00    	mov    0x2051a3(%rip),%eax        # 606168 <target_id>
  400fc5:	8d 78 01             	lea    0x1(%rax),%edi
  400fc8:	e8 53 fc ff ff       	callq  400c20 <srandom@plt>
  400fcd:	e8 7e fd ff ff       	callq  400d50 <random@plt>
  400fd2:	89 c7                	mov    %eax,%edi
  400fd4:	e8 ca 02 00 00       	callq  4012a3 <scramble>
  400fd9:	89 c3                	mov    %eax,%ebx
  400fdb:	85 ed                	test   %ebp,%ebp
  400fdd:	74 18                	je     400ff7 <initialize_target+0x67>
  400fdf:	bf 00 00 00 00       	mov    $0x0,%edi
  400fe4:	e8 57 fd ff ff       	callq  400d40 <time@plt>
  400fe9:	89 c7                	mov    %eax,%edi
  400feb:	e8 30 fc ff ff       	callq  400c20 <srandom@plt>
  400ff0:	e8 5b fd ff ff       	callq  400d50 <random@plt>
  400ff5:	eb 05                	jmp    400ffc <initialize_target+0x6c>
  400ff7:	b8 00 00 00 00       	mov    $0x0,%eax
  400ffc:	01 c3                	add    %eax,%ebx
  400ffe:	0f b7 db             	movzwl %bx,%ebx
  401001:	8d 04 dd 00 01 00 00 	lea    0x100(,%rbx,8),%eax
  401008:	89 c0                	mov    %eax,%eax
  40100a:	48 89 05 6f 59 20 00 	mov    %rax,0x20596f(%rip)        # 606980 <buf_offset>
  401011:	c6 05 f0 65 20 00 72 	movb   $0x72,0x2065f0(%rip)        # 607608 <target_prefix>
  401018:	83 3d 69 59 20 00 00 	cmpl   $0x0,0x205969(%rip)        # 606988 <notify>
  40101f:	0f 84 b1 00 00 00    	je     4010d6 <initialize_target+0x146>
  401025:	83 3d bc 59 20 00 00 	cmpl   $0x0,0x2059bc(%rip)        # 6069e8 <is_checker>
  40102c:	0f 85 a4 00 00 00    	jne    4010d6 <initialize_target+0x146>
  401032:	be 00 01 00 00       	mov    $0x100,%esi
  401037:	48 89 e7             	mov    %rsp,%rdi
  40103a:	e8 91 fd ff ff       	callq  400dd0 <gethostname@plt>
  40103f:	85 c0                	test   %eax,%eax
  401041:	74 25                	je     401068 <initialize_target+0xd8>
  401043:	bf 60 2e 40 00       	mov    $0x402e60,%edi
  401048:	e8 03 fc ff ff       	callq  400c50 <puts@plt>
  40104d:	bf 08 00 00 00       	mov    $0x8,%edi
  401052:	e8 99 fd ff ff       	callq  400df0 <exit@plt>
  401057:	48 89 e6             	mov    %rsp,%rsi
  40105a:	e8 a1 fb ff ff       	callq  400c00 <strcasecmp@plt>
  40105f:	85 c0                	test   %eax,%eax
  401061:	74 21                	je     401084 <initialize_target+0xf4>
  401063:	83 c3 01             	add    $0x1,%ebx
  401066:	eb 05                	jmp    40106d <initialize_target+0xdd>
  401068:	bb 00 00 00 00       	mov    $0x0,%ebx
  40106d:	48 63 c3             	movslq %ebx,%rax
  401070:	48 8b 3c c5 80 61 60 	mov    0x606180(,%rax,8),%rdi
  401077:	00 
  401078:	48 85 ff             	test   %rdi,%rdi
  40107b:	75 da                	jne    401057 <initialize_target+0xc7>
  40107d:	b8 00 00 00 00       	mov    $0x0,%eax
  401082:	eb 05                	jmp    401089 <initialize_target+0xf9>
  401084:	b8 01 00 00 00       	mov    $0x1,%eax
  401089:	85 c0                	test   %eax,%eax
  40108b:	75 17                	jne    4010a4 <initialize_target+0x114>
  40108d:	48 89 e6             	mov    %rsp,%rsi
  401090:	bf 98 2e 40 00       	mov    $0x402e98,%edi
  401095:	e8 e6 fb ff ff       	callq  400c80 <printf@plt>
  40109a:	bf 08 00 00 00       	mov    $0x8,%edi
  40109f:	e8 4c fd ff ff       	callq  400df0 <exit@plt>
  4010a4:	48 8d bc 24 00 01 00 	lea    0x100(%rsp),%rdi
  4010ab:	00 
  4010ac:	e8 a9 19 00 00       	callq  402a5a <init_driver>
  4010b1:	85 c0                	test   %eax,%eax
  4010b3:	79 21                	jns    4010d6 <initialize_target+0x146>
  4010b5:	48 8d b4 24 00 01 00 	lea    0x100(%rsp),%rsi
  4010bc:	00 
  4010bd:	bf d8 2e 40 00       	mov    $0x402ed8,%edi
  4010c2:	b8 00 00 00 00       	mov    $0x0,%eax
  4010c7:	e8 b4 fb ff ff       	callq  400c80 <printf@plt>
  4010cc:	bf 08 00 00 00       	mov    $0x8,%edi
  4010d1:	e8 1a fd ff ff       	callq  400df0 <exit@plt>
  4010d6:	48 81 c4 08 21 00 00 	add    $0x2108,%rsp
  4010dd:	5b                   	pop    %rbx
  4010de:	5d                   	pop    %rbp
  4010df:	c3                   	retq   

00000000004010e0 <main>:
  4010e0:	41 56                	push   %r14
  4010e2:	41 55                	push   %r13
  4010e4:	41 54                	push   %r12
  4010e6:	55                   	push   %rbp
  4010e7:	53                   	push   %rbx
  4010e8:	41 89 fc             	mov    %edi,%r12d
  4010eb:	48 89 f3             	mov    %rsi,%rbx
  4010ee:	be 81 1e 40 00       	mov    $0x401e81,%esi
  4010f3:	bf 0b 00 00 00       	mov    $0xb,%edi
  4010f8:	e8 e3 fb ff ff       	callq  400ce0 <signal@plt>
  4010fd:	be 33 1e 40 00       	mov    $0x401e33,%esi
  401102:	bf 07 00 00 00       	mov    $0x7,%edi
  401107:	e8 d4 fb ff ff       	callq  400ce0 <signal@plt>
  40110c:	be cf 1e 40 00       	mov    $0x401ecf,%esi
  401111:	bf 04 00 00 00       	mov    $0x4,%edi
  401116:	e8 c5 fb ff ff       	callq  400ce0 <signal@plt>
  40111b:	83 3d c6 58 20 00 00 	cmpl   $0x0,0x2058c6(%rip)        # 6069e8 <is_checker>
  401122:	74 20                	je     401144 <main+0x64>
  401124:	be 1d 1f 40 00       	mov    $0x401f1d,%esi
  401129:	bf 0e 00 00 00       	mov    $0xe,%edi
  40112e:	e8 ad fb ff ff       	callq  400ce0 <signal@plt>
  401133:	bf 05 00 00 00       	mov    $0x5,%edi
  401138:	e8 63 fb ff ff       	callq  400ca0 <alarm@plt>
  40113d:	bd a2 2f 40 00       	mov    $0x402fa2,%ebp
  401142:	eb 05                	jmp    401149 <main+0x69>
  401144:	bd 9d 2f 40 00       	mov    $0x402f9d,%ebp
  401149:	48 8b 05 50 58 20 00 	mov    0x205850(%rip),%rax        # 6069a0 <stdin@@GLIBC_2.2.5>
  401150:	48 89 05 79 58 20 00 	mov    %rax,0x205879(%rip)        # 6069d0 <infile>
  401157:	41 bd 00 00 00 00    	mov    $0x0,%r13d
  40115d:	41 be 00 00 00 00    	mov    $0x0,%r14d
  401163:	e9 b9 00 00 00       	jmpq   401221 <main+0x141>
  401168:	83 e8 61             	sub    $0x61,%eax
  40116b:	3c 10                	cmp    $0x10,%al
  40116d:	0f 87 93 00 00 00    	ja     401206 <main+0x126>
  401173:	0f b6 c0             	movzbl %al,%eax
  401176:	ff 24 c5 e8 2f 40 00 	jmpq   *0x402fe8(,%rax,8)
  40117d:	48 8b 3b             	mov    (%rbx),%rdi
  401180:	e8 8b fd ff ff       	callq  400f10 <usage>
  401185:	be 88 40 40 00       	mov    $0x404088,%esi
  40118a:	48 8b 3d 17 58 20 00 	mov    0x205817(%rip),%rdi        # 6069a8 <optarg@@GLIBC_2.2.5>
  401191:	e8 0a fc ff ff       	callq  400da0 <fopen@plt>
  401196:	48 89 05 33 58 20 00 	mov    %rax,0x205833(%rip)        # 6069d0 <infile>
  40119d:	48 85 c0             	test   %rax,%rax
  4011a0:	75 7f                	jne    401221 <main+0x141>
  4011a2:	48 8b 15 ff 57 20 00 	mov    0x2057ff(%rip),%rdx        # 6069a8 <optarg@@GLIBC_2.2.5>
  4011a9:	be aa 2f 40 00       	mov    $0x402faa,%esi
  4011ae:	48 8b 3d fb 57 20 00 	mov    0x2057fb(%rip),%rdi        # 6069b0 <stderr@@GLIBC_2.2.5>
  4011b5:	e8 46 fb ff ff       	callq  400d00 <fprintf@plt>
  4011ba:	b8 01 00 00 00       	mov    $0x1,%eax
  4011bf:	e9 d6 00 00 00       	jmpq   40129a <main+0x1ba>
  4011c4:	ba 10 00 00 00       	mov    $0x10,%edx
  4011c9:	be 00 00 00 00       	mov    $0x0,%esi
  4011ce:	48 8b 3d d3 57 20 00 	mov    0x2057d3(%rip),%rdi        # 6069a8 <optarg@@GLIBC_2.2.5>
  4011d5:	e8 e6 fb ff ff       	callq  400dc0 <strtoul@plt>
  4011da:	41 89 c6             	mov    %eax,%r14d
  4011dd:	eb 42                	jmp    401221 <main+0x141>
  4011df:	ba 0a 00 00 00       	mov    $0xa,%edx
  4011e4:	be 00 00 00 00       	mov    $0x0,%esi
  4011e9:	48 8b 3d b8 57 20 00 	mov    0x2057b8(%rip),%rdi        # 6069a8 <optarg@@GLIBC_2.2.5>
  4011f0:	e8 2b fb ff ff       	callq  400d20 <strtol@plt>
  4011f5:	41 89 c5             	mov    %eax,%r13d
  4011f8:	eb 27                	jmp    401221 <main+0x141>
  4011fa:	c7 05 84 57 20 00 00 	movl   $0x0,0x205784(%rip)        # 606988 <notify>
  401201:	00 00 00 
  401204:	eb 1b                	jmp    401221 <main+0x141>
  401206:	40 0f be f6          	movsbl %sil,%esi
  40120a:	bf c7 2f 40 00       	mov    $0x402fc7,%edi
  40120f:	b8 00 00 00 00       	mov    $0x0,%eax
  401214:	e8 67 fa ff ff       	callq  400c80 <printf@plt>
  401219:	48 8b 3b             	mov    (%rbx),%rdi
  40121c:	e8 ef fc ff ff       	callq  400f10 <usage>
  401221:	48 89 ea             	mov    %rbp,%rdx
  401224:	48 89 de             	mov    %rbx,%rsi
  401227:	44 89 e7             	mov    %r12d,%edi
  40122a:	e8 81 fb ff ff       	callq  400db0 <getopt@plt>
  40122f:	89 c6                	mov    %eax,%esi
  401231:	3c ff                	cmp    $0xff,%al
  401233:	0f 85 2f ff ff ff    	jne    401168 <main+0x88>
  401239:	be 01 00 00 00       	mov    $0x1,%esi
  40123e:	44 89 ef             	mov    %r13d,%edi
  401241:	e8 4a fd ff ff       	callq  400f90 <initialize_target>
  401246:	83 3d 9b 57 20 00 00 	cmpl   $0x0,0x20579b(%rip)        # 6069e8 <is_checker>
  40124d:	74 25                	je     401274 <main+0x194>
  40124f:	44 3b 35 8a 57 20 00 	cmp    0x20578a(%rip),%r14d        # 6069e0 <authkey>
  401256:	74 1c                	je     401274 <main+0x194>
  401258:	44 89 f6             	mov    %r14d,%esi
  40125b:	bf 00 2f 40 00       	mov    $0x402f00,%edi
  401260:	b8 00 00 00 00       	mov    $0x0,%eax
  401265:	e8 16 fa ff ff       	callq  400c80 <printf@plt>
  40126a:	b8 00 00 00 00       	mov    $0x0,%eax
  40126f:	e8 c7 08 00 00       	callq  401b3b <check_fail>
  401274:	8b 35 6a 57 20 00    	mov    0x20576a(%rip),%esi        # 6069e4 <cookie>
  40127a:	bf da 2f 40 00       	mov    $0x402fda,%edi
  40127f:	b8 00 00 00 00       	mov    $0x0,%eax
  401284:	e8 f7 f9 ff ff       	callq  400c80 <printf@plt>
  401289:	48 8b 3d f0 56 20 00 	mov    0x2056f0(%rip),%rdi        # 606980 <buf_offset>
  401290:	e8 d6 0c 00 00       	callq  401f6b <launch>
  401295:	b8 00 00 00 00       	mov    $0x0,%eax
  40129a:	5b                   	pop    %rbx
  40129b:	5d                   	pop    %rbp
  40129c:	41 5c                	pop    %r12
  40129e:	41 5d                	pop    %r13
  4012a0:	41 5e                	pop    %r14
  4012a2:	c3                   	retq   

00000000004012a3 <scramble>:
  4012a3:	b8 00 00 00 00       	mov    $0x0,%eax
  4012a8:	eb 11                	jmp    4012bb <scramble+0x18>
  4012aa:	69 c8 fd ed 00 00    	imul   $0xedfd,%eax,%ecx
  4012b0:	01 f9                	add    %edi,%ecx
  4012b2:	89 c2                	mov    %eax,%edx
  4012b4:	89 4c 94 d0          	mov    %ecx,-0x30(%rsp,%rdx,4)
  4012b8:	83 c0 01             	add    $0x1,%eax
  4012bb:	83 f8 09             	cmp    $0x9,%eax
  4012be:	76 ea                	jbe    4012aa <scramble+0x7>
  4012c0:	8b 44 24 d8          	mov    -0x28(%rsp),%eax
  4012c4:	69 c0 89 aa 00 00    	imul   $0xaa89,%eax,%eax
  4012ca:	89 44 24 d8          	mov    %eax,-0x28(%rsp)
  4012ce:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  4012d2:	69 c0 c3 bf 00 00    	imul   $0xbfc3,%eax,%eax
  4012d8:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  4012dc:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  4012e0:	69 c0 77 1b 00 00    	imul   $0x1b77,%eax,%eax
  4012e6:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  4012ea:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  4012ee:	69 c0 af b2 00 00    	imul   $0xb2af,%eax,%eax
  4012f4:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  4012f8:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  4012fc:	69 c0 8d 31 00 00    	imul   $0x318d,%eax,%eax
  401302:	89 44 24 e4          	mov    %eax,-0x1c(%rsp)
  401306:	8b 44 24 d4          	mov    -0x2c(%rsp),%eax
  40130a:	69 c0 4f 12 00 00    	imul   $0x124f,%eax,%eax
  401310:	89 44 24 d4          	mov    %eax,-0x2c(%rsp)
  401314:	8b 44 24 d8          	mov    -0x28(%rsp),%eax
  401318:	69 c0 24 4e 00 00    	imul   $0x4e24,%eax,%eax
  40131e:	89 44 24 d8          	mov    %eax,-0x28(%rsp)
  401322:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  401326:	69 c0 8d 3b 00 00    	imul   $0x3b8d,%eax,%eax
  40132c:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  401330:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  401334:	69 c0 fd 53 00 00    	imul   $0x53fd,%eax,%eax
  40133a:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  40133e:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  401342:	69 c0 2f a9 00 00    	imul   $0xa92f,%eax,%eax
  401348:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  40134c:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  401350:	69 c0 a5 1c 00 00    	imul   $0x1ca5,%eax,%eax
  401356:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  40135a:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  40135e:	69 c0 7e 8a 00 00    	imul   $0x8a7e,%eax,%eax
  401364:	89 44 24 e4          	mov    %eax,-0x1c(%rsp)
  401368:	8b 44 24 d8          	mov    -0x28(%rsp),%eax
  40136c:	69 c0 1c 46 00 00    	imul   $0x461c,%eax,%eax
  401372:	89 44 24 d8          	mov    %eax,-0x28(%rsp)
  401376:	8b 44 24 d0          	mov    -0x30(%rsp),%eax
  40137a:	69 c0 93 93 00 00    	imul   $0x9393,%eax,%eax
  401380:	89 44 24 d0          	mov    %eax,-0x30(%rsp)
  401384:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  401388:	69 c0 64 16 00 00    	imul   $0x1664,%eax,%eax
  40138e:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  401392:	8b 44 24 ec          	mov    -0x14(%rsp),%eax
  401396:	69 c0 01 a5 00 00    	imul   $0xa501,%eax,%eax
  40139c:	89 44 24 ec          	mov    %eax,-0x14(%rsp)
  4013a0:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  4013a4:	69 c0 5c 6e 00 00    	imul   $0x6e5c,%eax,%eax
  4013aa:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  4013ae:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  4013b2:	69 c0 8d f9 00 00    	imul   $0xf98d,%eax,%eax
  4013b8:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  4013bc:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  4013c0:	69 c0 a3 0d 00 00    	imul   $0xda3,%eax,%eax
  4013c6:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  4013ca:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  4013ce:	69 c0 44 28 00 00    	imul   $0x2844,%eax,%eax
  4013d4:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  4013d8:	8b 44 24 f4          	mov    -0xc(%rsp),%eax
  4013dc:	69 c0 25 cb 00 00    	imul   $0xcb25,%eax,%eax
  4013e2:	89 44 24 f4          	mov    %eax,-0xc(%rsp)
  4013e6:	8b 44 24 f4          	mov    -0xc(%rsp),%eax
  4013ea:	69 c0 a0 47 00 00    	imul   $0x47a0,%eax,%eax
  4013f0:	89 44 24 f4          	mov    %eax,-0xc(%rsp)
  4013f4:	8b 44 24 d4          	mov    -0x2c(%rsp),%eax
  4013f8:	69 c0 15 42 00 00    	imul   $0x4215,%eax,%eax
  4013fe:	89 44 24 d4          	mov    %eax,-0x2c(%rsp)
  401402:	8b 44 24 d8          	mov    -0x28(%rsp),%eax
  401406:	69 c0 72 aa 00 00    	imul   $0xaa72,%eax,%eax
  40140c:	89 44 24 d8          	mov    %eax,-0x28(%rsp)
  401410:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  401414:	69 c0 6a d4 00 00    	imul   $0xd46a,%eax,%eax
  40141a:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  40141e:	8b 44 24 ec          	mov    -0x14(%rsp),%eax
  401422:	69 c0 55 3d 00 00    	imul   $0x3d55,%eax,%eax
  401428:	89 44 24 ec          	mov    %eax,-0x14(%rsp)
  40142c:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  401430:	69 c0 87 43 00 00    	imul   $0x4387,%eax,%eax
  401436:	89 44 24 e4          	mov    %eax,-0x1c(%rsp)
  40143a:	8b 44 24 ec          	mov    -0x14(%rsp),%eax
  40143e:	69 c0 0c 74 00 00    	imul   $0x740c,%eax,%eax
  401444:	89 44 24 ec          	mov    %eax,-0x14(%rsp)
  401448:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  40144c:	69 c0 9b 73 00 00    	imul   $0x739b,%eax,%eax
  401452:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  401456:	8b 44 24 ec          	mov    -0x14(%rsp),%eax
  40145a:	69 c0 a7 40 00 00    	imul   $0x40a7,%eax,%eax
  401460:	89 44 24 ec          	mov    %eax,-0x14(%rsp)
  401464:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  401468:	69 c0 b0 0f 00 00    	imul   $0xfb0,%eax,%eax
  40146e:	89 44 24 e4          	mov    %eax,-0x1c(%rsp)
  401472:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  401476:	69 c0 ed 4a 00 00    	imul   $0x4aed,%eax,%eax
  40147c:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  401480:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  401484:	69 c0 6d 4a 00 00    	imul   $0x4a6d,%eax,%eax
  40148a:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  40148e:	8b 44 24 d4          	mov    -0x2c(%rsp),%eax
  401492:	69 c0 e0 07 00 00    	imul   $0x7e0,%eax,%eax
  401498:	89 44 24 d4          	mov    %eax,-0x2c(%rsp)
  40149c:	8b 44 24 ec          	mov    -0x14(%rsp),%eax
  4014a0:	69 c0 b6 f0 00 00    	imul   $0xf0b6,%eax,%eax
  4014a6:	89 44 24 ec          	mov    %eax,-0x14(%rsp)
  4014aa:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  4014ae:	69 c0 41 97 00 00    	imul   $0x9741,%eax,%eax
  4014b4:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  4014b8:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  4014bc:	69 c0 b8 6a 00 00    	imul   $0x6ab8,%eax,%eax
  4014c2:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  4014c6:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  4014ca:	69 c0 25 d5 00 00    	imul   $0xd525,%eax,%eax
  4014d0:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  4014d4:	8b 44 24 d4          	mov    -0x2c(%rsp),%eax
  4014d8:	69 c0 0e 62 00 00    	imul   $0x620e,%eax,%eax
  4014de:	89 44 24 d4          	mov    %eax,-0x2c(%rsp)
  4014e2:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  4014e6:	69 c0 e0 42 00 00    	imul   $0x42e0,%eax,%eax
  4014ec:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  4014f0:	8b 44 24 d4          	mov    -0x2c(%rsp),%eax
  4014f4:	69 c0 38 d3 00 00    	imul   $0xd338,%eax,%eax
  4014fa:	89 44 24 d4          	mov    %eax,-0x2c(%rsp)
  4014fe:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  401502:	69 c0 88 27 00 00    	imul   $0x2788,%eax,%eax
  401508:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  40150c:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  401510:	69 c0 09 48 00 00    	imul   $0x4809,%eax,%eax
  401516:	89 44 24 e4          	mov    %eax,-0x1c(%rsp)
  40151a:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  40151e:	69 c0 63 a0 00 00    	imul   $0xa063,%eax,%eax
  401524:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  401528:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  40152c:	69 c0 35 a3 00 00    	imul   $0xa335,%eax,%eax
  401532:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  401536:	8b 44 24 ec          	mov    -0x14(%rsp),%eax
  40153a:	69 c0 a4 45 00 00    	imul   $0x45a4,%eax,%eax
  401540:	89 44 24 ec          	mov    %eax,-0x14(%rsp)
  401544:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  401548:	69 c0 b4 c6 00 00    	imul   $0xc6b4,%eax,%eax
  40154e:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  401552:	8b 44 24 f4          	mov    -0xc(%rsp),%eax
  401556:	69 c0 f1 30 00 00    	imul   $0x30f1,%eax,%eax
  40155c:	89 44 24 f4          	mov    %eax,-0xc(%rsp)
  401560:	8b 44 24 d8          	mov    -0x28(%rsp),%eax
  401564:	69 c0 0b d5 00 00    	imul   $0xd50b,%eax,%eax
  40156a:	89 44 24 d8          	mov    %eax,-0x28(%rsp)
  40156e:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  401572:	69 c0 32 69 00 00    	imul   $0x6932,%eax,%eax
  401578:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  40157c:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  401580:	69 c0 f7 68 00 00    	imul   $0x68f7,%eax,%eax
  401586:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  40158a:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  40158e:	69 c0 9d 81 00 00    	imul   $0x819d,%eax,%eax
  401594:	89 44 24 e4          	mov    %eax,-0x1c(%rsp)
  401598:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  40159c:	69 c0 55 df 00 00    	imul   $0xdf55,%eax,%eax
  4015a2:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  4015a6:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  4015aa:	69 c0 7d 80 00 00    	imul   $0x807d,%eax,%eax
  4015b0:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  4015b4:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  4015b8:	69 c0 5a d6 00 00    	imul   $0xd65a,%eax,%eax
  4015be:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  4015c2:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  4015c6:	69 c0 dd 77 00 00    	imul   $0x77dd,%eax,%eax
  4015cc:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  4015d0:	8b 44 24 d4          	mov    -0x2c(%rsp),%eax
  4015d4:	69 c0 93 43 00 00    	imul   $0x4393,%eax,%eax
  4015da:	89 44 24 d4          	mov    %eax,-0x2c(%rsp)
  4015de:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  4015e2:	69 c0 53 c0 00 00    	imul   $0xc053,%eax,%eax
  4015e8:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  4015ec:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  4015f0:	69 c0 fa 3a 00 00    	imul   $0x3afa,%eax,%eax
  4015f6:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  4015fa:	8b 44 24 f4          	mov    -0xc(%rsp),%eax
  4015fe:	69 c0 41 47 00 00    	imul   $0x4741,%eax,%eax
  401604:	89 44 24 f4          	mov    %eax,-0xc(%rsp)
  401608:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  40160c:	69 c0 c1 41 00 00    	imul   $0x41c1,%eax,%eax
  401612:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  401616:	8b 44 24 ec          	mov    -0x14(%rsp),%eax
  40161a:	69 c0 14 91 00 00    	imul   $0x9114,%eax,%eax
  401620:	89 44 24 ec          	mov    %eax,-0x14(%rsp)
  401624:	8b 44 24 d8          	mov    -0x28(%rsp),%eax
  401628:	69 c0 76 1d 00 00    	imul   $0x1d76,%eax,%eax
  40162e:	89 44 24 d8          	mov    %eax,-0x28(%rsp)
  401632:	8b 44 24 f4          	mov    -0xc(%rsp),%eax
  401636:	69 c0 97 19 00 00    	imul   $0x1997,%eax,%eax
  40163c:	89 44 24 f4          	mov    %eax,-0xc(%rsp)
  401640:	8b 44 24 d4          	mov    -0x2c(%rsp),%eax
  401644:	69 c0 d8 fa 00 00    	imul   $0xfad8,%eax,%eax
  40164a:	89 44 24 d4          	mov    %eax,-0x2c(%rsp)
  40164e:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  401652:	69 c0 42 b8 00 00    	imul   $0xb842,%eax,%eax
  401658:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  40165c:	8b 44 24 d8          	mov    -0x28(%rsp),%eax
  401660:	69 c0 3d 83 00 00    	imul   $0x833d,%eax,%eax
  401666:	89 44 24 d8          	mov    %eax,-0x28(%rsp)
  40166a:	8b 44 24 f4          	mov    -0xc(%rsp),%eax
  40166e:	69 c0 7e 37 00 00    	imul   $0x377e,%eax,%eax
  401674:	89 44 24 f4          	mov    %eax,-0xc(%rsp)
  401678:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  40167c:	69 c0 a3 90 00 00    	imul   $0x90a3,%eax,%eax
  401682:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  401686:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  40168a:	69 c0 f7 a3 00 00    	imul   $0xa3f7,%eax,%eax
  401690:	89 44 24 e4          	mov    %eax,-0x1c(%rsp)
  401694:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  401698:	69 c0 5a 7b 00 00    	imul   $0x7b5a,%eax,%eax
  40169e:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  4016a2:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  4016a6:	69 c0 90 10 00 00    	imul   $0x1090,%eax,%eax
  4016ac:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  4016b0:	8b 44 24 f4          	mov    -0xc(%rsp),%eax
  4016b4:	69 c0 39 bb 00 00    	imul   $0xbb39,%eax,%eax
  4016ba:	89 44 24 f4          	mov    %eax,-0xc(%rsp)
  4016be:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  4016c2:	69 c0 8a 4a 00 00    	imul   $0x4a8a,%eax,%eax
  4016c8:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  4016cc:	8b 44 24 d4          	mov    -0x2c(%rsp),%eax
  4016d0:	69 c0 9a 31 00 00    	imul   $0x319a,%eax,%eax
  4016d6:	89 44 24 d4          	mov    %eax,-0x2c(%rsp)
  4016da:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  4016de:	69 c0 63 d4 00 00    	imul   $0xd463,%eax,%eax
  4016e4:	89 44 24 e4          	mov    %eax,-0x1c(%rsp)
  4016e8:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  4016ec:	69 c0 b0 1e 00 00    	imul   $0x1eb0,%eax,%eax
  4016f2:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  4016f6:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  4016fa:	69 c0 d1 7b 00 00    	imul   $0x7bd1,%eax,%eax
  401700:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  401704:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  401708:	69 c0 7e 27 00 00    	imul   $0x277e,%eax,%eax
  40170e:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  401712:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  401716:	69 c0 a7 2e 00 00    	imul   $0x2ea7,%eax,%eax
  40171c:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  401720:	8b 44 24 f4          	mov    -0xc(%rsp),%eax
  401724:	69 c0 31 18 00 00    	imul   $0x1831,%eax,%eax
  40172a:	89 44 24 f4          	mov    %eax,-0xc(%rsp)
  40172e:	8b 44 24 e4          	mov    -0x1c(%rsp),%eax
  401732:	69 c0 25 86 00 00    	imul   $0x8625,%eax,%eax
  401738:	89 44 24 e4          	mov    %eax,-0x1c(%rsp)
  40173c:	8b 44 24 ec          	mov    -0x14(%rsp),%eax
  401740:	69 c0 c9 20 00 00    	imul   $0x20c9,%eax,%eax
  401746:	89 44 24 ec          	mov    %eax,-0x14(%rsp)
  40174a:	8b 44 24 d0          	mov    -0x30(%rsp),%eax
  40174e:	69 c0 02 fe 00 00    	imul   $0xfe02,%eax,%eax
  401754:	89 44 24 d0          	mov    %eax,-0x30(%rsp)
  401758:	8b 44 24 dc          	mov    -0x24(%rsp),%eax
  40175c:	69 c0 9f 75 00 00    	imul   $0x759f,%eax,%eax
  401762:	89 44 24 dc          	mov    %eax,-0x24(%rsp)
  401766:	8b 44 24 f4          	mov    -0xc(%rsp),%eax
  40176a:	69 c0 6a 18 00 00    	imul   $0x186a,%eax,%eax
  401770:	89 44 24 f4          	mov    %eax,-0xc(%rsp)
  401774:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  401778:	69 c0 cd bd 00 00    	imul   $0xbdcd,%eax,%eax
  40177e:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  401782:	8b 44 24 e0          	mov    -0x20(%rsp),%eax
  401786:	69 c0 e7 af 00 00    	imul   $0xafe7,%eax,%eax
  40178c:	89 44 24 e0          	mov    %eax,-0x20(%rsp)
  401790:	8b 44 24 f4          	mov    -0xc(%rsp),%eax
  401794:	69 c0 de f9 00 00    	imul   $0xf9de,%eax,%eax
  40179a:	89 44 24 f4          	mov    %eax,-0xc(%rsp)
  40179e:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  4017a2:	69 c0 3a 6f 00 00    	imul   $0x6f3a,%eax,%eax
  4017a8:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  4017ac:	8b 44 24 d8          	mov    -0x28(%rsp),%eax
  4017b0:	69 c0 3c 8c 00 00    	imul   $0x8c3c,%eax,%eax
  4017b6:	89 44 24 d8          	mov    %eax,-0x28(%rsp)
  4017ba:	8b 44 24 f0          	mov    -0x10(%rsp),%eax
  4017be:	69 c0 d3 9e 00 00    	imul   $0x9ed3,%eax,%eax
  4017c4:	89 44 24 f0          	mov    %eax,-0x10(%rsp)
  4017c8:	8b 44 24 e8          	mov    -0x18(%rsp),%eax
  4017cc:	69 c0 16 a9 00 00    	imul   $0xa916,%eax,%eax
  4017d2:	89 44 24 e8          	mov    %eax,-0x18(%rsp)
  4017d6:	8b 44 24 d0          	mov    -0x30(%rsp),%eax
  4017da:	69 c0 12 e7 00 00    	imul   $0xe712,%eax,%eax
  4017e0:	89 44 24 d0          	mov    %eax,-0x30(%rsp)
  4017e4:	8b 44 24 f4          	mov    -0xc(%rsp),%eax
  4017e8:	69 c0 0c b0 00 00    	imul   $0xb00c,%eax,%eax
  4017ee:	89 44 24 f4          	mov    %eax,-0xc(%rsp)
  4017f2:	ba 00 00 00 00       	mov    $0x0,%edx
  4017f7:	b8 00 00 00 00       	mov    $0x0,%eax
  4017fc:	eb 0b                	jmp    401809 <scramble+0x566>
  4017fe:	89 d1                	mov    %edx,%ecx
  401800:	8b 4c 8c d0          	mov    -0x30(%rsp,%rcx,4),%ecx
  401804:	01 c8                	add    %ecx,%eax
  401806:	83 c2 01             	add    $0x1,%edx
  401809:	83 fa 09             	cmp    $0x9,%edx
  40180c:	76 f0                	jbe    4017fe <scramble+0x55b>
  40180e:	f3 c3                	repz retq 

0000000000401810 <getbuf>:
  401810:	48 83 ec 18          	sub    $0x18,%rsp
  401814:	48 89 e7             	mov    %rsp,%rdi
  401817:	e8 4e 03 00 00       	callq  401b6a <Gets>
  40181c:	b8 01 00 00 00       	mov    $0x1,%eax
  401821:	48 83 c4 18          	add    $0x18,%rsp
  401825:	c3                   	retq   

0000000000401826 <touch1>:
  401826:	48 83 ec 08          	sub    $0x8,%rsp
  40182a:	c7 05 a8 51 20 00 01 	movl   $0x1,0x2051a8(%rip)        # 6069dc <vlevel>
  401831:	00 00 00 
  401834:	bf d1 40 40 00       	mov    $0x4040d1,%edi
  401839:	e8 12 f4 ff ff       	callq  400c50 <puts@plt>
  40183e:	bf 01 00 00 00       	mov    $0x1,%edi
  401843:	e8 11 05 00 00       	callq  401d59 <validate>
  401848:	bf 00 00 00 00       	mov    $0x0,%edi
  40184d:	e8 9e f5 ff ff       	callq  400df0 <exit@plt>

0000000000401852 <touch2>:
  401852:	48 83 ec 08          	sub    $0x8,%rsp
  401856:	89 fe                	mov    %edi,%esi
  401858:	c7 05 7a 51 20 00 02 	movl   $0x2,0x20517a(%rip)        # 6069dc <vlevel>
  40185f:	00 00 00 
  401862:	3b 3d 7c 51 20 00    	cmp    0x20517c(%rip),%edi        # 6069e4 <cookie>
  401868:	75 1b                	jne    401885 <touch2+0x33>
  40186a:	bf f8 40 40 00       	mov    $0x4040f8,%edi
  40186f:	b8 00 00 00 00       	mov    $0x0,%eax
  401874:	e8 07 f4 ff ff       	callq  400c80 <printf@plt>
  401879:	bf 02 00 00 00       	mov    $0x2,%edi
  40187e:	e8 d6 04 00 00       	callq  401d59 <validate>
  401883:	eb 19                	jmp    40189e <touch2+0x4c>
  401885:	bf 20 41 40 00       	mov    $0x404120,%edi
  40188a:	b8 00 00 00 00       	mov    $0x0,%eax
  40188f:	e8 ec f3 ff ff       	callq  400c80 <printf@plt>
  401894:	bf 02 00 00 00       	mov    $0x2,%edi
  401899:	e8 6d 05 00 00       	callq  401e0b <fail>
  40189e:	bf 00 00 00 00       	mov    $0x0,%edi
  4018a3:	e8 48 f5 ff ff       	callq  400df0 <exit@plt>

00000000004018a8 <hexmatch>:
  4018a8:	41 54                	push   %r12
  4018aa:	55                   	push   %rbp
  4018ab:	53                   	push   %rbx
  4018ac:	48 83 ec 70          	sub    $0x70,%rsp
  4018b0:	41 89 fc             	mov    %edi,%r12d
  4018b3:	48 89 f5             	mov    %rsi,%rbp
  4018b6:	e8 95 f4 ff ff       	callq  400d50 <random@plt>
  4018bb:	48 89 c1             	mov    %rax,%rcx
  4018be:	48 ba 0b d7 a3 70 3d 	movabs $0xa3d70a3d70a3d70b,%rdx
  4018c5:	0a d7 a3 
  4018c8:	48 f7 ea             	imul   %rdx
  4018cb:	48 8d 04 0a          	lea    (%rdx,%rcx,1),%rax
  4018cf:	48 c1 f8 06          	sar    $0x6,%rax
  4018d3:	48 89 ce             	mov    %rcx,%rsi
  4018d6:	48 c1 fe 3f          	sar    $0x3f,%rsi
  4018da:	48 29 f0             	sub    %rsi,%rax
  4018dd:	48 8d 04 80          	lea    (%rax,%rax,4),%rax
  4018e1:	48 8d 04 80          	lea    (%rax,%rax,4),%rax
  4018e5:	48 c1 e0 02          	shl    $0x2,%rax
  4018e9:	48 29 c1             	sub    %rax,%rcx
  4018ec:	48 8d 1c 0c          	lea    (%rsp,%rcx,1),%rbx
  4018f0:	44 89 e2             	mov    %r12d,%edx
  4018f3:	be ee 40 40 00       	mov    $0x4040ee,%esi
  4018f8:	48 89 df             	mov    %rbx,%rdi
  4018fb:	b8 00 00 00 00       	mov    $0x0,%eax
  401900:	e8 db f4 ff ff       	callq  400de0 <sprintf@plt>
  401905:	ba 09 00 00 00       	mov    $0x9,%edx
  40190a:	48 89 de             	mov    %rbx,%rsi
  40190d:	48 89 ef             	mov    %rbp,%rdi
  401910:	e8 1b f3 ff ff       	callq  400c30 <strncmp@plt>
  401915:	85 c0                	test   %eax,%eax
  401917:	0f 94 c0             	sete   %al
  40191a:	0f b6 c0             	movzbl %al,%eax
  40191d:	48 83 c4 70          	add    $0x70,%rsp
  401921:	5b                   	pop    %rbx
  401922:	5d                   	pop    %rbp
  401923:	41 5c                	pop    %r12
  401925:	c3                   	retq   

0000000000401926 <touch3>:
  401926:	53                   	push   %rbx
  401927:	48 89 fb             	mov    %rdi,%rbx
  40192a:	c7 05 a8 50 20 00 03 	movl   $0x3,0x2050a8(%rip)        # 6069dc <vlevel>
  401931:	00 00 00 
  401934:	48 89 fe             	mov    %rdi,%rsi
  401937:	8b 3d a7 50 20 00    	mov    0x2050a7(%rip),%edi        # 6069e4 <cookie>
  40193d:	e8 66 ff ff ff       	callq  4018a8 <hexmatch>
  401942:	85 c0                	test   %eax,%eax
  401944:	74 1e                	je     401964 <touch3+0x3e>
  401946:	48 89 de             	mov    %rbx,%rsi
  401949:	bf 48 41 40 00       	mov    $0x404148,%edi
  40194e:	b8 00 00 00 00       	mov    $0x0,%eax
  401953:	e8 28 f3 ff ff       	callq  400c80 <printf@plt>
  401958:	bf 03 00 00 00       	mov    $0x3,%edi
  40195d:	e8 f7 03 00 00       	callq  401d59 <validate>
  401962:	eb 1c                	jmp    401980 <touch3+0x5a>
  401964:	48 89 de             	mov    %rbx,%rsi
  401967:	bf 70 41 40 00       	mov    $0x404170,%edi
  40196c:	b8 00 00 00 00       	mov    $0x0,%eax
  401971:	e8 0a f3 ff ff       	callq  400c80 <printf@plt>
  401976:	bf 03 00 00 00       	mov    $0x3,%edi
  40197b:	e8 8b 04 00 00       	callq  401e0b <fail>
  401980:	bf 00 00 00 00       	mov    $0x0,%edi
  401985:	e8 66 f4 ff ff       	callq  400df0 <exit@plt>

000000000040198a <test>:
  40198a:	48 83 ec 08          	sub    $0x8,%rsp
  40198e:	b8 00 00 00 00       	mov    $0x0,%eax
  401993:	e8 78 fe ff ff       	callq  401810 <getbuf>
  401998:	89 c6                	mov    %eax,%esi
  40199a:	bf 98 41 40 00       	mov    $0x404198,%edi
  40199f:	b8 00 00 00 00       	mov    $0x0,%eax
  4019a4:	e8 d7 f2 ff ff       	callq  400c80 <printf@plt>
  4019a9:	48 83 c4 08          	add    $0x8,%rsp
  4019ad:	c3                   	retq   

00000000004019ae <start_farm>:
  4019ae:	b8 01 00 00 00       	mov    $0x1,%eax
  4019b3:	c3                   	retq   

00000000004019b4 <getval_280>:
  4019b4:	b8 50 87 58 90       	mov    $0x90588750,%eax
  4019b9:	c3                   	retq   

00000000004019ba <setval_111>:
  4019ba:	c7 07 48 89 c7 c3    	movl   $0xc3c78948,(%rdi)
  4019c0:	c3                   	retq   

00000000004019c1 <setval_315>:
  4019c1:	c7 07 48 89 c7 91    	movl   $0x91c78948,(%rdi)
  4019c7:	c3                   	retq   

00000000004019c8 <addval_168>:
  4019c8:	8d 87 01 d8 90 c3    	lea    -0x3c6f27ff(%rdi),%eax
  4019ce:	c3                   	retq   

00000000004019cf <getval_230>:
  4019cf:	b8 48 89 c7 c1       	mov    $0xc1c78948,%eax
  4019d4:	c3                   	retq   

00000000004019d5 <addval_236>:
  4019d5:	8d 87 48 89 c7 c3    	lea    -0x3c3876b8(%rdi),%eax
  4019db:	c3                   	retq   

00000000004019dc <getval_317>:
  4019dc:	b8 50 58 90 90       	mov    $0x90905850,%eax
  4019e1:	c3                   	retq   

00000000004019e2 <setval_277>:
  4019e2:	c7 07 ec 18 90 c3    	movl   $0xc39018ec,(%rdi)
  4019e8:	c3                   	retq   

00000000004019e9 <mid_farm>:
  4019e9:	b8 01 00 00 00       	mov    $0x1,%eax
  4019ee:	c3                   	retq   

00000000004019ef <add_xy>:
  4019ef:	48 8d 04 37          	lea    (%rdi,%rsi,1),%rax
  4019f3:	c3                   	retq   

00000000004019f4 <setval_183>:
  4019f4:	c7 07 48 c9 e0 c3    	movl   $0xc3e0c948,(%rdi)
  4019fa:	c3                   	retq   

00000000004019fb <setval_227>:
  4019fb:	c7 07 48 89 e0 90    	movl   $0x90e08948,(%rdi)
  401a01:	c3                   	retq   

0000000000401a02 <getval_117>:
  401a02:	b8 a9 c2 20 c9       	mov    $0xc920c2a9,%eax
  401a07:	c3                   	retq   

0000000000401a08 <addval_468>:
  401a08:	8d 87 8d ce 84 d2    	lea    -0x2d7b3173(%rdi),%eax
  401a0e:	c3                   	retq   

0000000000401a0f <setval_243>:
  401a0f:	c7 07 4c 89 e0 c3    	movl   $0xc3e0894c,(%rdi)
  401a15:	c3                   	retq   

0000000000401a16 <getval_372>:
  401a16:	b8 99 c2 84 c0       	mov    $0xc084c299,%eax
  401a1b:	c3                   	retq   

0000000000401a1c <setval_265>:
  401a1c:	c7 07 81 c2 90 c3    	movl   $0xc390c281,(%rdi)
  401a22:	c3                   	retq   

0000000000401a23 <getval_161>:
  401a23:	b8 89 ce 18 c0       	mov    $0xc018ce89,%eax
  401a28:	c3                   	retq   

0000000000401a29 <addval_281>:
  401a29:	8d 87 89 ce 90 c1    	lea    -0x3e6f3177(%rdi),%eax
  401a2f:	c3                   	retq   

0000000000401a30 <getval_357>:
  401a30:	b8 89 d1 c4 db       	mov    $0xdbc4d189,%eax
  401a35:	c3                   	retq   

0000000000401a36 <addval_192>:
  401a36:	8d 87 89 c2 94 90    	lea    -0x6f6b3d77(%rdi),%eax
  401a3c:	c3                   	retq   

0000000000401a3d <setval_342>:
  401a3d:	c7 07 89 d1 92 c3    	movl   $0xc392d189,(%rdi)
  401a43:	c3                   	retq   

0000000000401a44 <addval_289>:
  401a44:	8d 87 c9 ce 20 c9    	lea    -0x36df3137(%rdi),%eax
  401a4a:	c3                   	retq   

0000000000401a4b <addval_276>:
  401a4b:	8d 87 89 ce 84 db    	lea    -0x247b3177(%rdi),%eax
  401a51:	c3                   	retq   

0000000000401a52 <addval_328>:
  401a52:	8d 87 48 89 e0 90    	lea    -0x6f1f76b8(%rdi),%eax
  401a58:	c3                   	retq   

0000000000401a59 <setval_316>:
  401a59:	c7 07 89 ce 18 db    	movl   $0xdb18ce89,(%rdi)
  401a5f:	c3                   	retq   

0000000000401a60 <addval_235>:
  401a60:	8d 87 e0 8b c2 c3    	lea    -0x3c3d7420(%rdi),%eax
  401a66:	c3                   	retq   

0000000000401a67 <getval_283>:
  401a67:	b8 48 89 e0 91       	mov    $0x91e08948,%eax
  401a6c:	c3                   	retq   

0000000000401a6d <getval_162>:
  401a6d:	b8 82 89 ce 90       	mov    $0x90ce8982,%eax
  401a72:	c3                   	retq   

0000000000401a73 <addval_199>:
  401a73:	8d 87 48 89 e0 92    	lea    -0x6d1f76b8(%rdi),%eax
  401a79:	c3                   	retq   

0000000000401a7a <addval_324>:
  401a7a:	8d 87 89 d1 18 db    	lea    -0x24e72e77(%rdi),%eax
  401a80:	c3                   	retq   

0000000000401a81 <getval_196>:
  401a81:	b8 89 ce 60 c0       	mov    $0xc060ce89,%eax
  401a86:	c3                   	retq   

0000000000401a87 <setval_318>:
  401a87:	c7 07 88 d1 38 c9    	movl   $0xc938d188,(%rdi)
  401a8d:	c3                   	retq   

0000000000401a8e <getval_482>:
  401a8e:	b8 89 d1 08 db       	mov    $0xdb08d189,%eax
  401a93:	c3                   	retq   

0000000000401a94 <addval_294>:
  401a94:	8d 87 48 8b e0 90    	lea    -0x6f1f74b8(%rdi),%eax
  401a9a:	c3                   	retq   

0000000000401a9b <addval_275>:
  401a9b:	8d 87 99 d1 84 c0    	lea    -0x3f7b2e67(%rdi),%eax
  401aa1:	c3                   	retq   

0000000000401aa2 <addval_237>:
  401aa2:	8d 87 b8 89 c2 90    	lea    -0x6f3d7648(%rdi),%eax
  401aa8:	c3                   	retq   

0000000000401aa9 <addval_436>:
  401aa9:	8d 87 89 d1 48 db    	lea    -0x24b72e77(%rdi),%eax
  401aaf:	c3                   	retq   

0000000000401ab0 <setval_433>:
  401ab0:	c7 07 89 c2 38 c9    	movl   $0xc938c289,(%rdi)
  401ab6:	c3                   	retq   

0000000000401ab7 <setval_193>:
  401ab7:	c7 07 c9 c2 08 d2    	movl   $0xd208c2c9,(%rdi)
  401abd:	c3                   	retq   

0000000000401abe <getval_473>:
  401abe:	b8 89 d1 90 c3       	mov    $0xc390d189,%eax
  401ac3:	c3                   	retq   

0000000000401ac4 <getval_125>:
  401ac4:	b8 48 89 e0 c7       	mov    $0xc7e08948,%eax
  401ac9:	c3                   	retq   

0000000000401aca <end_farm>:
  401aca:	b8 01 00 00 00       	mov    $0x1,%eax
  401acf:	c3                   	retq   

0000000000401ad0 <save_char>:
  401ad0:	8b 05 2e 5b 20 00    	mov    0x205b2e(%rip),%eax        # 607604 <gets_cnt>
  401ad6:	3d ff 03 00 00       	cmp    $0x3ff,%eax
  401adb:	7f 49                	jg     401b26 <save_char+0x56>
  401add:	8d 14 40             	lea    (%rax,%rax,2),%edx
  401ae0:	89 f9                	mov    %edi,%ecx
  401ae2:	c0 e9 04             	shr    $0x4,%cl
  401ae5:	83 e1 0f             	and    $0xf,%ecx
  401ae8:	0f b6 b1 c0 44 40 00 	movzbl 0x4044c0(%rcx),%esi
  401aef:	48 63 ca             	movslq %edx,%rcx
  401af2:	40 88 b1 00 6a 60 00 	mov    %sil,0x606a00(%rcx)
  401af9:	8d 4a 01             	lea    0x1(%rdx),%ecx
  401afc:	83 e7 0f             	and    $0xf,%edi
  401aff:	0f b6 b7 c0 44 40 00 	movzbl 0x4044c0(%rdi),%esi
  401b06:	48 63 c9             	movslq %ecx,%rcx
  401b09:	40 88 b1 00 6a 60 00 	mov    %sil,0x606a00(%rcx)
  401b10:	83 c2 02             	add    $0x2,%edx
  401b13:	48 63 d2             	movslq %edx,%rdx
  401b16:	c6 82 00 6a 60 00 20 	movb   $0x20,0x606a00(%rdx)
  401b1d:	83 c0 01             	add    $0x1,%eax
  401b20:	89 05 de 5a 20 00    	mov    %eax,0x205ade(%rip)        # 607604 <gets_cnt>
  401b26:	f3 c3                	repz retq 

0000000000401b28 <save_term>:
  401b28:	8b 05 d6 5a 20 00    	mov    0x205ad6(%rip),%eax        # 607604 <gets_cnt>
  401b2e:	8d 04 40             	lea    (%rax,%rax,2),%eax
  401b31:	48 98                	cltq   
  401b33:	c6 80 00 6a 60 00 00 	movb   $0x0,0x606a00(%rax)
  401b3a:	c3                   	retq   

0000000000401b3b <check_fail>:
  401b3b:	48 83 ec 08          	sub    $0x8,%rsp
  401b3f:	0f be 35 c2 5a 20 00 	movsbl 0x205ac2(%rip),%esi        # 607608 <target_prefix>
  401b46:	b9 00 6a 60 00       	mov    $0x606a00,%ecx
  401b4b:	8b 15 87 4e 20 00    	mov    0x204e87(%rip),%edx        # 6069d8 <check_level>
  401b51:	bf bb 41 40 00       	mov    $0x4041bb,%edi
  401b56:	b8 00 00 00 00       	mov    $0x0,%eax
  401b5b:	e8 20 f1 ff ff       	callq  400c80 <printf@plt>
  401b60:	bf 01 00 00 00       	mov    $0x1,%edi
  401b65:	e8 86 f2 ff ff       	callq  400df0 <exit@plt>

0000000000401b6a <Gets>:
  401b6a:	41 54                	push   %r12
  401b6c:	55                   	push   %rbp
  401b6d:	53                   	push   %rbx
  401b6e:	49 89 fc             	mov    %rdi,%r12
  401b71:	c7 05 89 5a 20 00 00 	movl   $0x0,0x205a89(%rip)        # 607604 <gets_cnt>
  401b78:	00 00 00 
  401b7b:	48 89 fb             	mov    %rdi,%rbx
  401b7e:	eb 11                	jmp    401b91 <Gets+0x27>
  401b80:	48 8d 6b 01          	lea    0x1(%rbx),%rbp
  401b84:	88 03                	mov    %al,(%rbx)
  401b86:	0f b6 f8             	movzbl %al,%edi
  401b89:	e8 42 ff ff ff       	callq  401ad0 <save_char>
  401b8e:	48 89 eb             	mov    %rbp,%rbx
  401b91:	48 8b 3d 38 4e 20 00 	mov    0x204e38(%rip),%rdi        # 6069d0 <infile>
  401b98:	e8 c3 f1 ff ff       	callq  400d60 <_IO_getc@plt>
  401b9d:	83 f8 ff             	cmp    $0xffffffff,%eax
  401ba0:	74 05                	je     401ba7 <Gets+0x3d>
  401ba2:	83 f8 0a             	cmp    $0xa,%eax
  401ba5:	75 d9                	jne    401b80 <Gets+0x16>
  401ba7:	c6 03 00             	movb   $0x0,(%rbx)
  401baa:	b8 00 00 00 00       	mov    $0x0,%eax
  401baf:	e8 74 ff ff ff       	callq  401b28 <save_term>
  401bb4:	4c 89 e0             	mov    %r12,%rax
  401bb7:	5b                   	pop    %rbx
  401bb8:	5d                   	pop    %rbp
  401bb9:	41 5c                	pop    %r12
  401bbb:	c3                   	retq   

0000000000401bbc <notify_server>:
  401bbc:	83 3d 25 4e 20 00 00 	cmpl   $0x0,0x204e25(%rip)        # 6069e8 <is_checker>
  401bc3:	0f 85 8e 01 00 00    	jne    401d57 <notify_server+0x19b>
  401bc9:	53                   	push   %rbx
  401bca:	48 81 ec 10 40 00 00 	sub    $0x4010,%rsp
  401bd1:	89 fb                	mov    %edi,%ebx
  401bd3:	8b 05 2b 5a 20 00    	mov    0x205a2b(%rip),%eax        # 607604 <gets_cnt>
  401bd9:	83 c0 64             	add    $0x64,%eax
  401bdc:	3d 00 20 00 00       	cmp    $0x2000,%eax
  401be1:	7e 19                	jle    401bfc <notify_server+0x40>
  401be3:	bf f0 42 40 00       	mov    $0x4042f0,%edi
  401be8:	b8 00 00 00 00       	mov    $0x0,%eax
  401bed:	e8 8e f0 ff ff       	callq  400c80 <printf@plt>
  401bf2:	bf 01 00 00 00       	mov    $0x1,%edi
  401bf7:	e8 f4 f1 ff ff       	callq  400df0 <exit@plt>
  401bfc:	44 0f be 0d 04 5a 20 	movsbl 0x205a04(%rip),%r9d        # 607608 <target_prefix>
  401c03:	00 
  401c04:	83 3d 7d 4d 20 00 00 	cmpl   $0x0,0x204d7d(%rip)        # 606988 <notify>
  401c0b:	74 09                	je     401c16 <notify_server+0x5a>
  401c0d:	44 8b 05 cc 4d 20 00 	mov    0x204dcc(%rip),%r8d        # 6069e0 <authkey>
  401c14:	eb 06                	jmp    401c1c <notify_server+0x60>
  401c16:	41 b8 ff ff ff ff    	mov    $0xffffffff,%r8d
  401c1c:	85 db                	test   %ebx,%ebx
  401c1e:	74 07                	je     401c27 <notify_server+0x6b>
  401c20:	b9 d1 41 40 00       	mov    $0x4041d1,%ecx
  401c25:	eb 05                	jmp    401c2c <notify_server+0x70>
  401c27:	b9 d6 41 40 00       	mov    $0x4041d6,%ecx
  401c2c:	48 c7 44 24 08 00 6a 	movq   $0x606a00,0x8(%rsp)
  401c33:	60 00 
  401c35:	89 34 24             	mov    %esi,(%rsp)
  401c38:	8b 15 2a 45 20 00    	mov    0x20452a(%rip),%edx        # 606168 <target_id>
  401c3e:	be db 41 40 00       	mov    $0x4041db,%esi
  401c43:	48 8d bc 24 10 20 00 	lea    0x2010(%rsp),%rdi
  401c4a:	00 
  401c4b:	b8 00 00 00 00       	mov    $0x0,%eax
  401c50:	e8 8b f1 ff ff       	callq  400de0 <sprintf@plt>
  401c55:	83 3d 2c 4d 20 00 00 	cmpl   $0x0,0x204d2c(%rip)        # 606988 <notify>
  401c5c:	74 78                	je     401cd6 <notify_server+0x11a>
  401c5e:	85 db                	test   %ebx,%ebx
  401c60:	74 68                	je     401cca <notify_server+0x10e>
  401c62:	4c 8d 4c 24 10       	lea    0x10(%rsp),%r9
  401c67:	41 b8 00 00 00 00    	mov    $0x0,%r8d
  401c6d:	48 8d 8c 24 10 20 00 	lea    0x2010(%rsp),%rcx
  401c74:	00 
  401c75:	48 8b 15 f4 44 20 00 	mov    0x2044f4(%rip),%rdx        # 606170 <lab>
  401c7c:	48 8b 35 f5 44 20 00 	mov    0x2044f5(%rip),%rsi        # 606178 <course>
  401c83:	48 8b 3d d6 44 20 00 	mov    0x2044d6(%rip),%rdi        # 606160 <user_id>
  401c8a:	e8 94 0f 00 00       	callq  402c23 <driver_post>
  401c8f:	85 c0                	test   %eax,%eax
  401c91:	79 1e                	jns    401cb1 <notify_server+0xf5>
  401c93:	48 8d 74 24 10       	lea    0x10(%rsp),%rsi
  401c98:	bf f7 41 40 00       	mov    $0x4041f7,%edi
  401c9d:	b8 00 00 00 00       	mov    $0x0,%eax
  401ca2:	e8 d9 ef ff ff       	callq  400c80 <printf@plt>
  401ca7:	bf 01 00 00 00       	mov    $0x1,%edi
  401cac:	e8 3f f1 ff ff       	callq  400df0 <exit@plt>
  401cb1:	bf 20 43 40 00       	mov    $0x404320,%edi
  401cb6:	e8 95 ef ff ff       	callq  400c50 <puts@plt>
  401cbb:	bf 03 42 40 00       	mov    $0x404203,%edi
  401cc0:	e8 8b ef ff ff       	callq  400c50 <puts@plt>
  401cc5:	e9 85 00 00 00       	jmpq   401d4f <notify_server+0x193>
  401cca:	bf 0d 42 40 00       	mov    $0x40420d,%edi
  401ccf:	e8 7c ef ff ff       	callq  400c50 <puts@plt>
  401cd4:	eb 79                	jmp    401d4f <notify_server+0x193>
  401cd6:	85 db                	test   %ebx,%ebx
  401cd8:	74 08                	je     401ce2 <notify_server+0x126>
  401cda:	be d1 41 40 00       	mov    $0x4041d1,%esi
  401cdf:	90                   	nop
  401ce0:	eb 05                	jmp    401ce7 <notify_server+0x12b>
  401ce2:	be d6 41 40 00       	mov    $0x4041d6,%esi
  401ce7:	bf 58 43 40 00       	mov    $0x404358,%edi
  401cec:	b8 00 00 00 00       	mov    $0x0,%eax
  401cf1:	e8 8a ef ff ff       	callq  400c80 <printf@plt>
  401cf6:	48 8b 35 63 44 20 00 	mov    0x204463(%rip),%rsi        # 606160 <user_id>
  401cfd:	bf 14 42 40 00       	mov    $0x404214,%edi
  401d02:	b8 00 00 00 00       	mov    $0x0,%eax
  401d07:	e8 74 ef ff ff       	callq  400c80 <printf@plt>
  401d0c:	48 8b 35 65 44 20 00 	mov    0x204465(%rip),%rsi        # 606178 <course>
  401d13:	bf 21 42 40 00       	mov    $0x404221,%edi
  401d18:	b8 00 00 00 00       	mov    $0x0,%eax
  401d1d:	e8 5e ef ff ff       	callq  400c80 <printf@plt>
  401d22:	48 8b 35 47 44 20 00 	mov    0x204447(%rip),%rsi        # 606170 <lab>
  401d29:	bf 2d 42 40 00       	mov    $0x40422d,%edi
  401d2e:	b8 00 00 00 00       	mov    $0x0,%eax
  401d33:	e8 48 ef ff ff       	callq  400c80 <printf@plt>
  401d38:	48 8d b4 24 10 20 00 	lea    0x2010(%rsp),%rsi
  401d3f:	00 
  401d40:	bf 36 42 40 00       	mov    $0x404236,%edi
  401d45:	b8 00 00 00 00       	mov    $0x0,%eax
  401d4a:	e8 31 ef ff ff       	callq  400c80 <printf@plt>
  401d4f:	48 81 c4 10 40 00 00 	add    $0x4010,%rsp
  401d56:	5b                   	pop    %rbx
  401d57:	f3 c3                	repz retq 

0000000000401d59 <validate>:
  401d59:	53                   	push   %rbx
  401d5a:	89 fb                	mov    %edi,%ebx
  401d5c:	83 3d 85 4c 20 00 00 	cmpl   $0x0,0x204c85(%rip)        # 6069e8 <is_checker>
  401d63:	74 60                	je     401dc5 <validate+0x6c>
  401d65:	39 3d 71 4c 20 00    	cmp    %edi,0x204c71(%rip)        # 6069dc <vlevel>
  401d6b:	74 14                	je     401d81 <validate+0x28>
  401d6d:	bf 42 42 40 00       	mov    $0x404242,%edi
  401d72:	e8 d9 ee ff ff       	callq  400c50 <puts@plt>
  401d77:	b8 00 00 00 00       	mov    $0x0,%eax
  401d7c:	e8 ba fd ff ff       	callq  401b3b <check_fail>
  401d81:	8b 35 51 4c 20 00    	mov    0x204c51(%rip),%esi        # 6069d8 <check_level>
  401d87:	39 fe                	cmp    %edi,%esi
  401d89:	74 1b                	je     401da6 <validate+0x4d>
  401d8b:	89 fa                	mov    %edi,%edx
  401d8d:	bf 80 43 40 00       	mov    $0x404380,%edi
  401d92:	b8 00 00 00 00       	mov    $0x0,%eax
  401d97:	e8 e4 ee ff ff       	callq  400c80 <printf@plt>
  401d9c:	b8 00 00 00 00       	mov    $0x0,%eax
  401da1:	e8 95 fd ff ff       	callq  401b3b <check_fail>
  401da6:	0f be 35 5b 58 20 00 	movsbl 0x20585b(%rip),%esi        # 607608 <target_prefix>
  401dad:	b9 00 6a 60 00       	mov    $0x606a00,%ecx
  401db2:	89 fa                	mov    %edi,%edx
  401db4:	bf 60 42 40 00       	mov    $0x404260,%edi
  401db9:	b8 00 00 00 00       	mov    $0x0,%eax
  401dbe:	e8 bd ee ff ff       	callq  400c80 <printf@plt>
  401dc3:	eb 44                	jmp    401e09 <validate+0xb0>
  401dc5:	39 3d 11 4c 20 00    	cmp    %edi,0x204c11(%rip)        # 6069dc <vlevel>
  401dcb:	74 18                	je     401de5 <validate+0x8c>
  401dcd:	bf 42 42 40 00       	mov    $0x404242,%edi
  401dd2:	e8 79 ee ff ff       	callq  400c50 <puts@plt>
  401dd7:	89 de                	mov    %ebx,%esi
  401dd9:	bf 00 00 00 00       	mov    $0x0,%edi
  401dde:	e8 d9 fd ff ff       	callq  401bbc <notify_server>
  401de3:	eb 24                	jmp    401e09 <validate+0xb0>
  401de5:	0f be 15 1c 58 20 00 	movsbl 0x20581c(%rip),%edx        # 607608 <target_prefix>
  401dec:	89 fe                	mov    %edi,%esi
  401dee:	bf a8 43 40 00       	mov    $0x4043a8,%edi
  401df3:	b8 00 00 00 00       	mov    $0x0,%eax
  401df8:	e8 83 ee ff ff       	callq  400c80 <printf@plt>
  401dfd:	89 de                	mov    %ebx,%esi
  401dff:	bf 01 00 00 00       	mov    $0x1,%edi
  401e04:	e8 b3 fd ff ff       	callq  401bbc <notify_server>
  401e09:	5b                   	pop    %rbx
  401e0a:	c3                   	retq   

0000000000401e0b <fail>:
  401e0b:	48 83 ec 08          	sub    $0x8,%rsp
  401e0f:	83 3d d2 4b 20 00 00 	cmpl   $0x0,0x204bd2(%rip)        # 6069e8 <is_checker>
  401e16:	74 0a                	je     401e22 <fail+0x17>
  401e18:	b8 00 00 00 00       	mov    $0x0,%eax
  401e1d:	e8 19 fd ff ff       	callq  401b3b <check_fail>
  401e22:	89 fe                	mov    %edi,%esi
  401e24:	bf 00 00 00 00       	mov    $0x0,%edi
  401e29:	e8 8e fd ff ff       	callq  401bbc <notify_server>
  401e2e:	48 83 c4 08          	add    $0x8,%rsp
  401e32:	c3                   	retq   

0000000000401e33 <bushandler>:
  401e33:	48 83 ec 08          	sub    $0x8,%rsp
  401e37:	83 3d aa 4b 20 00 00 	cmpl   $0x0,0x204baa(%rip)        # 6069e8 <is_checker>
  401e3e:	74 14                	je     401e54 <bushandler+0x21>
  401e40:	bf 75 42 40 00       	mov    $0x404275,%edi
  401e45:	e8 06 ee ff ff       	callq  400c50 <puts@plt>
  401e4a:	b8 00 00 00 00       	mov    $0x0,%eax
  401e4f:	e8 e7 fc ff ff       	callq  401b3b <check_fail>
  401e54:	bf e0 43 40 00       	mov    $0x4043e0,%edi
  401e59:	e8 f2 ed ff ff       	callq  400c50 <puts@plt>
  401e5e:	bf 7f 42 40 00       	mov    $0x40427f,%edi
  401e63:	e8 e8 ed ff ff       	callq  400c50 <puts@plt>
  401e68:	be 00 00 00 00       	mov    $0x0,%esi
  401e6d:	bf 00 00 00 00       	mov    $0x0,%edi
  401e72:	e8 45 fd ff ff       	callq  401bbc <notify_server>
  401e77:	bf 01 00 00 00       	mov    $0x1,%edi
  401e7c:	e8 6f ef ff ff       	callq  400df0 <exit@plt>

0000000000401e81 <seghandler>:
  401e81:	48 83 ec 08          	sub    $0x8,%rsp
  401e85:	83 3d 5c 4b 20 00 00 	cmpl   $0x0,0x204b5c(%rip)        # 6069e8 <is_checker>
  401e8c:	74 14                	je     401ea2 <seghandler+0x21>
  401e8e:	bf 95 42 40 00       	mov    $0x404295,%edi
  401e93:	e8 b8 ed ff ff       	callq  400c50 <puts@plt>
  401e98:	b8 00 00 00 00       	mov    $0x0,%eax
  401e9d:	e8 99 fc ff ff       	callq  401b3b <check_fail>
  401ea2:	bf 00 44 40 00       	mov    $0x404400,%edi
  401ea7:	e8 a4 ed ff ff       	callq  400c50 <puts@plt>
  401eac:	bf 7f 42 40 00       	mov    $0x40427f,%edi
  401eb1:	e8 9a ed ff ff       	callq  400c50 <puts@plt>
  401eb6:	be 00 00 00 00       	mov    $0x0,%esi
  401ebb:	bf 00 00 00 00       	mov    $0x0,%edi
  401ec0:	e8 f7 fc ff ff       	callq  401bbc <notify_server>
  401ec5:	bf 01 00 00 00       	mov    $0x1,%edi
  401eca:	e8 21 ef ff ff       	callq  400df0 <exit@plt>

0000000000401ecf <illegalhandler>:
  401ecf:	48 83 ec 08          	sub    $0x8,%rsp
  401ed3:	83 3d 0e 4b 20 00 00 	cmpl   $0x0,0x204b0e(%rip)        # 6069e8 <is_checker>
  401eda:	74 14                	je     401ef0 <illegalhandler+0x21>
  401edc:	bf a8 42 40 00       	mov    $0x4042a8,%edi
  401ee1:	e8 6a ed ff ff       	callq  400c50 <puts@plt>
  401ee6:	b8 00 00 00 00       	mov    $0x0,%eax
  401eeb:	e8 4b fc ff ff       	callq  401b3b <check_fail>
  401ef0:	bf 28 44 40 00       	mov    $0x404428,%edi
  401ef5:	e8 56 ed ff ff       	callq  400c50 <puts@plt>
  401efa:	bf 7f 42 40 00       	mov    $0x40427f,%edi
  401eff:	e8 4c ed ff ff       	callq  400c50 <puts@plt>
  401f04:	be 00 00 00 00       	mov    $0x0,%esi
  401f09:	bf 00 00 00 00       	mov    $0x0,%edi
  401f0e:	e8 a9 fc ff ff       	callq  401bbc <notify_server>
  401f13:	bf 01 00 00 00       	mov    $0x1,%edi
  401f18:	e8 d3 ee ff ff       	callq  400df0 <exit@plt>

0000000000401f1d <sigalrmhandler>:
  401f1d:	48 83 ec 08          	sub    $0x8,%rsp
  401f21:	83 3d c0 4a 20 00 00 	cmpl   $0x0,0x204ac0(%rip)        # 6069e8 <is_checker>
  401f28:	74 14                	je     401f3e <sigalrmhandler+0x21>
  401f2a:	bf bc 42 40 00       	mov    $0x4042bc,%edi
  401f2f:	e8 1c ed ff ff       	callq  400c50 <puts@plt>
  401f34:	b8 00 00 00 00       	mov    $0x0,%eax
  401f39:	e8 fd fb ff ff       	callq  401b3b <check_fail>
  401f3e:	be 05 00 00 00       	mov    $0x5,%esi
  401f43:	bf 58 44 40 00       	mov    $0x404458,%edi
  401f48:	b8 00 00 00 00       	mov    $0x0,%eax
  401f4d:	e8 2e ed ff ff       	callq  400c80 <printf@plt>
  401f52:	be 00 00 00 00       	mov    $0x0,%esi
  401f57:	bf 00 00 00 00       	mov    $0x0,%edi
  401f5c:	e8 5b fc ff ff       	callq  401bbc <notify_server>
  401f61:	bf 01 00 00 00       	mov    $0x1,%edi
  401f66:	e8 85 ee ff ff       	callq  400df0 <exit@plt>

0000000000401f6b <launch>:
  401f6b:	55                   	push   %rbp
  401f6c:	48 89 e5             	mov    %rsp,%rbp
  401f6f:	48 89 fa             	mov    %rdi,%rdx
  401f72:	48 8d 47 1e          	lea    0x1e(%rdi),%rax
  401f76:	48 83 e0 f0          	and    $0xfffffffffffffff0,%rax
  401f7a:	48 29 c4             	sub    %rax,%rsp
  401f7d:	48 8d 7c 24 0f       	lea    0xf(%rsp),%rdi
  401f82:	48 83 e7 f0          	and    $0xfffffffffffffff0,%rdi
  401f86:	be f4 00 00 00       	mov    $0xf4,%esi
  401f8b:	e8 00 ed ff ff       	callq  400c90 <memset@plt>
  401f90:	48 8b 05 09 4a 20 00 	mov    0x204a09(%rip),%rax        # 6069a0 <stdin@@GLIBC_2.2.5>
  401f97:	48 39 05 32 4a 20 00 	cmp    %rax,0x204a32(%rip)        # 6069d0 <infile>
  401f9e:	75 0f                	jne    401faf <launch+0x44>
  401fa0:	bf c4 42 40 00       	mov    $0x4042c4,%edi
  401fa5:	b8 00 00 00 00       	mov    $0x0,%eax
  401faa:	e8 d1 ec ff ff       	callq  400c80 <printf@plt>
  401faf:	c7 05 23 4a 20 00 00 	movl   $0x0,0x204a23(%rip)        # 6069dc <vlevel>
  401fb6:	00 00 00 
  401fb9:	b8 00 00 00 00       	mov    $0x0,%eax
  401fbe:	e8 c7 f9 ff ff       	callq  40198a <test>
  401fc3:	83 3d 1e 4a 20 00 00 	cmpl   $0x0,0x204a1e(%rip)        # 6069e8 <is_checker>
  401fca:	74 14                	je     401fe0 <launch+0x75>
  401fcc:	bf d1 42 40 00       	mov    $0x4042d1,%edi
  401fd1:	e8 7a ec ff ff       	callq  400c50 <puts@plt>
  401fd6:	b8 00 00 00 00       	mov    $0x0,%eax
  401fdb:	e8 5b fb ff ff       	callq  401b3b <check_fail>
  401fe0:	bf dc 42 40 00       	mov    $0x4042dc,%edi
  401fe5:	e8 66 ec ff ff       	callq  400c50 <puts@plt>
  401fea:	c9                   	leaveq 
  401feb:	c3                   	retq   

0000000000401fec <stable_launch>:
  401fec:	53                   	push   %rbx
  401fed:	48 89 3d d4 49 20 00 	mov    %rdi,0x2049d4(%rip)        # 6069c8 <global_offset>
  401ff4:	41 b9 00 00 00 00    	mov    $0x0,%r9d
  401ffa:	41 b8 00 00 00 00    	mov    $0x0,%r8d
  402000:	b9 32 01 00 00       	mov    $0x132,%ecx
  402005:	ba 07 00 00 00       	mov    $0x7,%edx
  40200a:	be 00 00 10 00       	mov    $0x100000,%esi
  40200f:	bf 00 60 58 55       	mov    $0x55586000,%edi
  402014:	e8 57 ec ff ff       	callq  400c70 <mmap@plt>
  402019:	48 89 c3             	mov    %rax,%rbx
  40201c:	48 3d 00 60 58 55    	cmp    $0x55586000,%rax
  402022:	74 32                	je     402056 <stable_launch+0x6a>
  402024:	be 00 00 10 00       	mov    $0x100000,%esi
  402029:	48 89 c7             	mov    %rax,%rdi
  40202c:	e8 4f ed ff ff       	callq  400d80 <munmap@plt>
  402031:	ba 00 60 58 55       	mov    $0x55586000,%edx
  402036:	be 90 44 40 00       	mov    $0x404490,%esi
  40203b:	48 8b 3d 6e 49 20 00 	mov    0x20496e(%rip),%rdi        # 6069b0 <stderr@@GLIBC_2.2.5>
  402042:	b8 00 00 00 00       	mov    $0x0,%eax
  402047:	e8 b4 ec ff ff       	callq  400d00 <fprintf@plt>
  40204c:	bf 01 00 00 00       	mov    $0x1,%edi
  402051:	e8 9a ed ff ff       	callq  400df0 <exit@plt>
  402056:	48 8d 90 f8 ff 0f 00 	lea    0xffff8(%rax),%rdx
  40205d:	48 89 15 ac 55 20 00 	mov    %rdx,0x2055ac(%rip)        # 607610 <stack_top>
  402064:	48 89 e0             	mov    %rsp,%rax
  402067:	48 89 d4             	mov    %rdx,%rsp
  40206a:	48 89 c2             	mov    %rax,%rdx
  40206d:	48 89 15 4c 49 20 00 	mov    %rdx,0x20494c(%rip)        # 6069c0 <global_save_stack>
  402074:	48 8b 3d 4d 49 20 00 	mov    0x20494d(%rip),%rdi        # 6069c8 <global_offset>
  40207b:	e8 eb fe ff ff       	callq  401f6b <launch>
  402080:	48 8b 05 39 49 20 00 	mov    0x204939(%rip),%rax        # 6069c0 <global_save_stack>
  402087:	48 89 c4             	mov    %rax,%rsp
  40208a:	be 00 00 10 00       	mov    $0x100000,%esi
  40208f:	48 89 df             	mov    %rbx,%rdi
  402092:	e8 e9 ec ff ff       	callq  400d80 <munmap@plt>
  402097:	5b                   	pop    %rbx
  402098:	c3                   	retq   
  402099:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

00000000004020a0 <rio_readinitb>:
  4020a0:	89 37                	mov    %esi,(%rdi)
  4020a2:	c7 47 04 00 00 00 00 	movl   $0x0,0x4(%rdi)
  4020a9:	48 8d 47 10          	lea    0x10(%rdi),%rax
  4020ad:	48 89 47 08          	mov    %rax,0x8(%rdi)
  4020b1:	c3                   	retq   

00000000004020b2 <sigalrm_handler>:
  4020b2:	48 83 ec 08          	sub    $0x8,%rsp
  4020b6:	ba 00 00 00 00       	mov    $0x0,%edx
  4020bb:	be d0 44 40 00       	mov    $0x4044d0,%esi
  4020c0:	48 8b 3d e9 48 20 00 	mov    0x2048e9(%rip),%rdi        # 6069b0 <stderr@@GLIBC_2.2.5>
  4020c7:	b8 00 00 00 00       	mov    $0x0,%eax
  4020cc:	e8 2f ec ff ff       	callq  400d00 <fprintf@plt>
  4020d1:	bf 01 00 00 00       	mov    $0x1,%edi
  4020d6:	e8 15 ed ff ff       	callq  400df0 <exit@plt>

00000000004020db <urlencode>:
  4020db:	41 54                	push   %r12
  4020dd:	55                   	push   %rbp
  4020de:	53                   	push   %rbx
  4020df:	48 83 ec 10          	sub    $0x10,%rsp
  4020e3:	48 89 fb             	mov    %rdi,%rbx
  4020e6:	48 89 f5             	mov    %rsi,%rbp
  4020e9:	b8 00 00 00 00       	mov    $0x0,%eax
  4020ee:	48 c7 c1 ff ff ff ff 	mov    $0xffffffffffffffff,%rcx
  4020f5:	f2 ae                	repnz scas %es:(%rdi),%al
  4020f7:	48 f7 d1             	not    %rcx
  4020fa:	8d 41 ff             	lea    -0x1(%rcx),%eax
  4020fd:	e9 93 00 00 00       	jmpq   402195 <urlencode+0xba>
  402102:	0f b6 13             	movzbl (%rbx),%edx
  402105:	80 fa 2a             	cmp    $0x2a,%dl
  402108:	0f 94 c1             	sete   %cl
  40210b:	80 fa 2d             	cmp    $0x2d,%dl
  40210e:	0f 94 c0             	sete   %al
  402111:	08 c1                	or     %al,%cl
  402113:	75 1f                	jne    402134 <urlencode+0x59>
  402115:	80 fa 2e             	cmp    $0x2e,%dl
  402118:	74 1a                	je     402134 <urlencode+0x59>
  40211a:	80 fa 5f             	cmp    $0x5f,%dl
  40211d:	74 15                	je     402134 <urlencode+0x59>
  40211f:	8d 42 d0             	lea    -0x30(%rdx),%eax
  402122:	3c 09                	cmp    $0x9,%al
  402124:	76 0e                	jbe    402134 <urlencode+0x59>
  402126:	8d 42 bf             	lea    -0x41(%rdx),%eax
  402129:	3c 19                	cmp    $0x19,%al
  40212b:	76 07                	jbe    402134 <urlencode+0x59>
  40212d:	8d 42 9f             	lea    -0x61(%rdx),%eax
  402130:	3c 19                	cmp    $0x19,%al
  402132:	77 09                	ja     40213d <urlencode+0x62>
  402134:	88 55 00             	mov    %dl,0x0(%rbp)
  402137:	48 8d 6d 01          	lea    0x1(%rbp),%rbp
  40213b:	eb 51                	jmp    40218e <urlencode+0xb3>
  40213d:	80 fa 20             	cmp    $0x20,%dl
  402140:	75 0a                	jne    40214c <urlencode+0x71>
  402142:	c6 45 00 2b          	movb   $0x2b,0x0(%rbp)
  402146:	48 8d 6d 01          	lea    0x1(%rbp),%rbp
  40214a:	eb 42                	jmp    40218e <urlencode+0xb3>
  40214c:	8d 42 e0             	lea    -0x20(%rdx),%eax
  40214f:	3c 5f                	cmp    $0x5f,%al
  402151:	0f 96 c1             	setbe  %cl
  402154:	80 fa 09             	cmp    $0x9,%dl
  402157:	0f 94 c0             	sete   %al
  40215a:	08 c1                	or     %al,%cl
  40215c:	74 45                	je     4021a3 <urlencode+0xc8>
  40215e:	0f b6 d2             	movzbl %dl,%edx
  402161:	be 68 45 40 00       	mov    $0x404568,%esi
  402166:	48 89 e7             	mov    %rsp,%rdi
  402169:	b8 00 00 00 00       	mov    $0x0,%eax
  40216e:	e8 6d ec ff ff       	callq  400de0 <sprintf@plt>
  402173:	0f b6 04 24          	movzbl (%rsp),%eax
  402177:	88 45 00             	mov    %al,0x0(%rbp)
  40217a:	0f b6 44 24 01       	movzbl 0x1(%rsp),%eax
  40217f:	88 45 01             	mov    %al,0x1(%rbp)
  402182:	0f b6 44 24 02       	movzbl 0x2(%rsp),%eax
  402187:	88 45 02             	mov    %al,0x2(%rbp)
  40218a:	48 8d 6d 03          	lea    0x3(%rbp),%rbp
  40218e:	48 83 c3 01          	add    $0x1,%rbx
  402192:	44 89 e0             	mov    %r12d,%eax
  402195:	44 8d 60 ff          	lea    -0x1(%rax),%r12d
  402199:	85 c0                	test   %eax,%eax
  40219b:	0f 85 61 ff ff ff    	jne    402102 <urlencode+0x27>
  4021a1:	eb 05                	jmp    4021a8 <urlencode+0xcd>
  4021a3:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  4021a8:	48 83 c4 10          	add    $0x10,%rsp
  4021ac:	5b                   	pop    %rbx
  4021ad:	5d                   	pop    %rbp
  4021ae:	41 5c                	pop    %r12
  4021b0:	c3                   	retq   

00000000004021b1 <rio_writen>:
  4021b1:	41 55                	push   %r13
  4021b3:	41 54                	push   %r12
  4021b5:	55                   	push   %rbp
  4021b6:	53                   	push   %rbx
  4021b7:	48 83 ec 08          	sub    $0x8,%rsp
  4021bb:	41 89 fc             	mov    %edi,%r12d
  4021be:	48 89 f5             	mov    %rsi,%rbp
  4021c1:	49 89 d5             	mov    %rdx,%r13
  4021c4:	48 89 d3             	mov    %rdx,%rbx
  4021c7:	eb 28                	jmp    4021f1 <rio_writen+0x40>
  4021c9:	48 89 da             	mov    %rbx,%rdx
  4021cc:	48 89 ee             	mov    %rbp,%rsi
  4021cf:	44 89 e7             	mov    %r12d,%edi
  4021d2:	e8 89 ea ff ff       	callq  400c60 <write@plt>
  4021d7:	48 85 c0             	test   %rax,%rax
  4021da:	7f 0f                	jg     4021eb <rio_writen+0x3a>
  4021dc:	e8 2f ea ff ff       	callq  400c10 <__errno_location@plt>
  4021e1:	83 38 04             	cmpl   $0x4,(%rax)
  4021e4:	75 15                	jne    4021fb <rio_writen+0x4a>
  4021e6:	b8 00 00 00 00       	mov    $0x0,%eax
  4021eb:	48 29 c3             	sub    %rax,%rbx
  4021ee:	48 01 c5             	add    %rax,%rbp
  4021f1:	48 85 db             	test   %rbx,%rbx
  4021f4:	75 d3                	jne    4021c9 <rio_writen+0x18>
  4021f6:	4c 89 e8             	mov    %r13,%rax
  4021f9:	eb 07                	jmp    402202 <rio_writen+0x51>
  4021fb:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  402202:	48 83 c4 08          	add    $0x8,%rsp
  402206:	5b                   	pop    %rbx
  402207:	5d                   	pop    %rbp
  402208:	41 5c                	pop    %r12
  40220a:	41 5d                	pop    %r13
  40220c:	c3                   	retq   

000000000040220d <rio_read>:
  40220d:	41 56                	push   %r14
  40220f:	41 55                	push   %r13
  402211:	41 54                	push   %r12
  402213:	55                   	push   %rbp
  402214:	53                   	push   %rbx
  402215:	48 89 fb             	mov    %rdi,%rbx
  402218:	49 89 f6             	mov    %rsi,%r14
  40221b:	49 89 d5             	mov    %rdx,%r13
  40221e:	4c 8d 67 10          	lea    0x10(%rdi),%r12
  402222:	eb 2a                	jmp    40224e <rio_read+0x41>
  402224:	ba 00 20 00 00       	mov    $0x2000,%edx
  402229:	4c 89 e6             	mov    %r12,%rsi
  40222c:	8b 3b                	mov    (%rbx),%edi
  40222e:	e8 8d ea ff ff       	callq  400cc0 <read@plt>
  402233:	89 43 04             	mov    %eax,0x4(%rbx)
  402236:	85 c0                	test   %eax,%eax
  402238:	79 0c                	jns    402246 <rio_read+0x39>
  40223a:	e8 d1 e9 ff ff       	callq  400c10 <__errno_location@plt>
  40223f:	83 38 04             	cmpl   $0x4,(%rax)
  402242:	74 0a                	je     40224e <rio_read+0x41>
  402244:	eb 37                	jmp    40227d <rio_read+0x70>
  402246:	85 c0                	test   %eax,%eax
  402248:	74 3c                	je     402286 <rio_read+0x79>
  40224a:	4c 89 63 08          	mov    %r12,0x8(%rbx)
  40224e:	8b 6b 04             	mov    0x4(%rbx),%ebp
  402251:	85 ed                	test   %ebp,%ebp
  402253:	7e cf                	jle    402224 <rio_read+0x17>
  402255:	89 e8                	mov    %ebp,%eax
  402257:	4c 39 e8             	cmp    %r13,%rax
  40225a:	72 03                	jb     40225f <rio_read+0x52>
  40225c:	44 89 ed             	mov    %r13d,%ebp
  40225f:	4c 63 e5             	movslq %ebp,%r12
  402262:	48 8b 73 08          	mov    0x8(%rbx),%rsi
  402266:	4c 89 e2             	mov    %r12,%rdx
  402269:	4c 89 f7             	mov    %r14,%rdi
  40226c:	e8 bf ea ff ff       	callq  400d30 <memcpy@plt>
  402271:	4c 01 63 08          	add    %r12,0x8(%rbx)
  402275:	29 6b 04             	sub    %ebp,0x4(%rbx)
  402278:	4c 89 e0             	mov    %r12,%rax
  40227b:	eb 0e                	jmp    40228b <rio_read+0x7e>
  40227d:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  402284:	eb 05                	jmp    40228b <rio_read+0x7e>
  402286:	b8 00 00 00 00       	mov    $0x0,%eax
  40228b:	5b                   	pop    %rbx
  40228c:	5d                   	pop    %rbp
  40228d:	41 5c                	pop    %r12
  40228f:	41 5d                	pop    %r13
  402291:	41 5e                	pop    %r14
  402293:	c3                   	retq   

0000000000402294 <rio_readlineb>:
  402294:	41 55                	push   %r13
  402296:	41 54                	push   %r12
  402298:	55                   	push   %rbp
  402299:	53                   	push   %rbx
  40229a:	48 83 ec 18          	sub    $0x18,%rsp
  40229e:	49 89 fd             	mov    %rdi,%r13
  4022a1:	48 89 f5             	mov    %rsi,%rbp
  4022a4:	49 89 d4             	mov    %rdx,%r12
  4022a7:	bb 01 00 00 00       	mov    $0x1,%ebx
  4022ac:	eb 3d                	jmp    4022eb <rio_readlineb+0x57>
  4022ae:	ba 01 00 00 00       	mov    $0x1,%edx
  4022b3:	48 8d 74 24 0f       	lea    0xf(%rsp),%rsi
  4022b8:	4c 89 ef             	mov    %r13,%rdi
  4022bb:	e8 4d ff ff ff       	callq  40220d <rio_read>
  4022c0:	83 f8 01             	cmp    $0x1,%eax
  4022c3:	75 12                	jne    4022d7 <rio_readlineb+0x43>
  4022c5:	48 8d 55 01          	lea    0x1(%rbp),%rdx
  4022c9:	0f b6 44 24 0f       	movzbl 0xf(%rsp),%eax
  4022ce:	88 45 00             	mov    %al,0x0(%rbp)
  4022d1:	3c 0a                	cmp    $0xa,%al
  4022d3:	75 0f                	jne    4022e4 <rio_readlineb+0x50>
  4022d5:	eb 1b                	jmp    4022f2 <rio_readlineb+0x5e>
  4022d7:	85 c0                	test   %eax,%eax
  4022d9:	75 23                	jne    4022fe <rio_readlineb+0x6a>
  4022db:	48 83 fb 01          	cmp    $0x1,%rbx
  4022df:	90                   	nop
  4022e0:	75 13                	jne    4022f5 <rio_readlineb+0x61>
  4022e2:	eb 23                	jmp    402307 <rio_readlineb+0x73>
  4022e4:	48 83 c3 01          	add    $0x1,%rbx
  4022e8:	48 89 d5             	mov    %rdx,%rbp
  4022eb:	4c 39 e3             	cmp    %r12,%rbx
  4022ee:	72 be                	jb     4022ae <rio_readlineb+0x1a>
  4022f0:	eb 03                	jmp    4022f5 <rio_readlineb+0x61>
  4022f2:	48 89 d5             	mov    %rdx,%rbp
  4022f5:	c6 45 00 00          	movb   $0x0,0x0(%rbp)
  4022f9:	48 89 d8             	mov    %rbx,%rax
  4022fc:	eb 0e                	jmp    40230c <rio_readlineb+0x78>
  4022fe:	48 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%rax
  402305:	eb 05                	jmp    40230c <rio_readlineb+0x78>
  402307:	b8 00 00 00 00       	mov    $0x0,%eax
  40230c:	48 83 c4 18          	add    $0x18,%rsp
  402310:	5b                   	pop    %rbx
  402311:	5d                   	pop    %rbp
  402312:	41 5c                	pop    %r12
  402314:	41 5d                	pop    %r13
  402316:	c3                   	retq   

0000000000402317 <submitr>:
  402317:	41 57                	push   %r15
  402319:	41 56                	push   %r14
  40231b:	41 55                	push   %r13
  40231d:	41 54                	push   %r12
  40231f:	55                   	push   %rbp
  402320:	53                   	push   %rbx
  402321:	48 81 ec 48 a0 00 00 	sub    $0xa048,%rsp
  402328:	49 89 fc             	mov    %rdi,%r12
  40232b:	89 74 24 04          	mov    %esi,0x4(%rsp)
  40232f:	49 89 d7             	mov    %rdx,%r15
  402332:	49 89 ce             	mov    %rcx,%r14
  402335:	4c 89 44 24 08       	mov    %r8,0x8(%rsp)
  40233a:	4d 89 cd             	mov    %r9,%r13
  40233d:	48 8b 9c 24 80 a0 00 	mov    0xa080(%rsp),%rbx
  402344:	00 
  402345:	c7 84 24 1c 20 00 00 	movl   $0x0,0x201c(%rsp)
  40234c:	00 00 00 00 
  402350:	ba 00 00 00 00       	mov    $0x0,%edx
  402355:	be 01 00 00 00       	mov    $0x1,%esi
  40235a:	bf 02 00 00 00       	mov    $0x2,%edi
  40235f:	e8 ac ea ff ff       	callq  400e10 <socket@plt>
  402364:	89 c5                	mov    %eax,%ebp
  402366:	85 c0                	test   %eax,%eax
  402368:	79 4e                	jns    4023b8 <submitr+0xa1>
  40236a:	48 b8 45 72 72 6f 72 	movabs $0x43203a726f727245,%rax
  402371:	3a 20 43 
  402374:	48 89 03             	mov    %rax,(%rbx)
  402377:	48 b8 6c 69 65 6e 74 	movabs $0x6e7520746e65696c,%rax
  40237e:	20 75 6e 
  402381:	48 89 43 08          	mov    %rax,0x8(%rbx)
  402385:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  40238c:	74 6f 20 
  40238f:	48 89 43 10          	mov    %rax,0x10(%rbx)
  402393:	48 b8 63 72 65 61 74 	movabs $0x7320657461657263,%rax
  40239a:	65 20 73 
  40239d:	48 89 43 18          	mov    %rax,0x18(%rbx)
  4023a1:	c7 43 20 6f 63 6b 65 	movl   $0x656b636f,0x20(%rbx)
  4023a8:	66 c7 43 24 74 00    	movw   $0x74,0x24(%rbx)
  4023ae:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  4023b3:	e9 68 06 00 00       	jmpq   402a20 <submitr+0x709>
  4023b8:	4c 89 e7             	mov    %r12,%rdi
  4023bb:	e8 30 e9 ff ff       	callq  400cf0 <gethostbyname@plt>
  4023c0:	48 85 c0             	test   %rax,%rax
  4023c3:	75 67                	jne    40242c <submitr+0x115>
  4023c5:	48 b8 45 72 72 6f 72 	movabs $0x44203a726f727245,%rax
  4023cc:	3a 20 44 
  4023cf:	48 89 03             	mov    %rax,(%rbx)
  4023d2:	48 b8 4e 53 20 69 73 	movabs $0x6e7520736920534e,%rax
  4023d9:	20 75 6e 
  4023dc:	48 89 43 08          	mov    %rax,0x8(%rbx)
  4023e0:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  4023e7:	74 6f 20 
  4023ea:	48 89 43 10          	mov    %rax,0x10(%rbx)
  4023ee:	48 b8 72 65 73 6f 6c 	movabs $0x2065766c6f736572,%rax
  4023f5:	76 65 20 
  4023f8:	48 89 43 18          	mov    %rax,0x18(%rbx)
  4023fc:	48 b8 73 65 72 76 65 	movabs $0x6120726576726573,%rax
  402403:	72 20 61 
  402406:	48 89 43 20          	mov    %rax,0x20(%rbx)
  40240a:	c7 43 28 64 64 72 65 	movl   $0x65726464,0x28(%rbx)
  402411:	66 c7 43 2c 73 73    	movw   $0x7373,0x2c(%rbx)
  402417:	c6 43 2e 00          	movb   $0x0,0x2e(%rbx)
  40241b:	89 ef                	mov    %ebp,%edi
  40241d:	e8 8e e8 ff ff       	callq  400cb0 <close@plt>
  402422:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  402427:	e9 f4 05 00 00       	jmpq   402a20 <submitr+0x709>
  40242c:	48 c7 84 24 30 a0 00 	movq   $0x0,0xa030(%rsp)
  402433:	00 00 00 00 00 
  402438:	48 c7 84 24 38 a0 00 	movq   $0x0,0xa038(%rsp)
  40243f:	00 00 00 00 00 
  402444:	66 c7 84 24 30 a0 00 	movw   $0x2,0xa030(%rsp)
  40244b:	00 02 00 
  40244e:	48 8b 48 18          	mov    0x18(%rax),%rcx
  402452:	48 63 50 14          	movslq 0x14(%rax),%rdx
  402456:	48 8d b4 24 34 a0 00 	lea    0xa034(%rsp),%rsi
  40245d:	00 
  40245e:	48 8b 39             	mov    (%rcx),%rdi
  402461:	e8 2a e9 ff ff       	callq  400d90 <bcopy@plt>
  402466:	0f b7 44 24 04       	movzwl 0x4(%rsp),%eax
  40246b:	66 c1 c8 08          	ror    $0x8,%ax
  40246f:	66 89 84 24 32 a0 00 	mov    %ax,0xa032(%rsp)
  402476:	00 
  402477:	ba 10 00 00 00       	mov    $0x10,%edx
  40247c:	48 8d b4 24 30 a0 00 	lea    0xa030(%rsp),%rsi
  402483:	00 
  402484:	89 ef                	mov    %ebp,%edi
  402486:	e8 75 e9 ff ff       	callq  400e00 <connect@plt>
  40248b:	85 c0                	test   %eax,%eax
  40248d:	79 59                	jns    4024e8 <submitr+0x1d1>
  40248f:	48 b8 45 72 72 6f 72 	movabs $0x55203a726f727245,%rax
  402496:	3a 20 55 
  402499:	48 89 03             	mov    %rax,(%rbx)
  40249c:	48 b8 6e 61 62 6c 65 	movabs $0x6f7420656c62616e,%rax
  4024a3:	20 74 6f 
  4024a6:	48 89 43 08          	mov    %rax,0x8(%rbx)
  4024aa:	48 b8 20 63 6f 6e 6e 	movabs $0x7463656e6e6f6320,%rax
  4024b1:	65 63 74 
  4024b4:	48 89 43 10          	mov    %rax,0x10(%rbx)
  4024b8:	48 b8 20 74 6f 20 74 	movabs $0x20656874206f7420,%rax
  4024bf:	68 65 20 
  4024c2:	48 89 43 18          	mov    %rax,0x18(%rbx)
  4024c6:	c7 43 20 73 65 72 76 	movl   $0x76726573,0x20(%rbx)
  4024cd:	66 c7 43 24 65 72    	movw   $0x7265,0x24(%rbx)
  4024d3:	c6 43 26 00          	movb   $0x0,0x26(%rbx)
  4024d7:	89 ef                	mov    %ebp,%edi
  4024d9:	e8 d2 e7 ff ff       	callq  400cb0 <close@plt>
  4024de:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  4024e3:	e9 38 05 00 00       	jmpq   402a20 <submitr+0x709>
  4024e8:	48 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%rdx
  4024ef:	4c 89 ef             	mov    %r13,%rdi
  4024f2:	b8 00 00 00 00       	mov    $0x0,%eax
  4024f7:	48 89 d1             	mov    %rdx,%rcx
  4024fa:	f2 ae                	repnz scas %es:(%rdi),%al
  4024fc:	48 f7 d1             	not    %rcx
  4024ff:	48 89 ce             	mov    %rcx,%rsi
  402502:	4c 89 ff             	mov    %r15,%rdi
  402505:	48 89 d1             	mov    %rdx,%rcx
  402508:	f2 ae                	repnz scas %es:(%rdi),%al
  40250a:	48 f7 d1             	not    %rcx
  40250d:	49 89 c8             	mov    %rcx,%r8
  402510:	4c 89 f7             	mov    %r14,%rdi
  402513:	48 89 d1             	mov    %rdx,%rcx
  402516:	f2 ae                	repnz scas %es:(%rdi),%al
  402518:	49 29 c8             	sub    %rcx,%r8
  40251b:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
  402520:	48 89 d1             	mov    %rdx,%rcx
  402523:	f2 ae                	repnz scas %es:(%rdi),%al
  402525:	49 29 c8             	sub    %rcx,%r8
  402528:	48 8d 44 76 fd       	lea    -0x3(%rsi,%rsi,2),%rax
  40252d:	49 8d 44 00 7b       	lea    0x7b(%r8,%rax,1),%rax
  402532:	48 3d 00 20 00 00    	cmp    $0x2000,%rax
  402538:	76 72                	jbe    4025ac <submitr+0x295>
  40253a:	48 b8 45 72 72 6f 72 	movabs $0x52203a726f727245,%rax
  402541:	3a 20 52 
  402544:	48 89 03             	mov    %rax,(%rbx)
  402547:	48 b8 65 73 75 6c 74 	movabs $0x747320746c757365,%rax
  40254e:	20 73 74 
  402551:	48 89 43 08          	mov    %rax,0x8(%rbx)
  402555:	48 b8 72 69 6e 67 20 	movabs $0x6f6f7420676e6972,%rax
  40255c:	74 6f 6f 
  40255f:	48 89 43 10          	mov    %rax,0x10(%rbx)
  402563:	48 b8 20 6c 61 72 67 	movabs $0x202e656772616c20,%rax
  40256a:	65 2e 20 
  40256d:	48 89 43 18          	mov    %rax,0x18(%rbx)
  402571:	48 b8 49 6e 63 72 65 	movabs $0x6573616572636e49,%rax
  402578:	61 73 65 
  40257b:	48 89 43 20          	mov    %rax,0x20(%rbx)
  40257f:	48 b8 20 53 55 42 4d 	movabs $0x5254494d42555320,%rax
  402586:	49 54 52 
  402589:	48 89 43 28          	mov    %rax,0x28(%rbx)
  40258d:	48 b8 5f 4d 41 58 42 	movabs $0x46554258414d5f,%rax
  402594:	55 46 00 
  402597:	48 89 43 30          	mov    %rax,0x30(%rbx)
  40259b:	89 ef                	mov    %ebp,%edi
  40259d:	e8 0e e7 ff ff       	callq  400cb0 <close@plt>
  4025a2:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  4025a7:	e9 74 04 00 00       	jmpq   402a20 <submitr+0x709>
  4025ac:	48 8d b4 24 20 40 00 	lea    0x4020(%rsp),%rsi
  4025b3:	00 
  4025b4:	b9 00 04 00 00       	mov    $0x400,%ecx
  4025b9:	b8 00 00 00 00       	mov    $0x0,%eax
  4025be:	48 89 f7             	mov    %rsi,%rdi
  4025c1:	f3 48 ab             	rep stos %rax,%es:(%rdi)
  4025c4:	4c 89 ef             	mov    %r13,%rdi
  4025c7:	e8 0f fb ff ff       	callq  4020db <urlencode>
  4025cc:	85 c0                	test   %eax,%eax
  4025ce:	0f 89 8a 00 00 00    	jns    40265e <submitr+0x347>
  4025d4:	48 b8 45 72 72 6f 72 	movabs $0x52203a726f727245,%rax
  4025db:	3a 20 52 
  4025de:	48 89 03             	mov    %rax,(%rbx)
  4025e1:	48 b8 65 73 75 6c 74 	movabs $0x747320746c757365,%rax
  4025e8:	20 73 74 
  4025eb:	48 89 43 08          	mov    %rax,0x8(%rbx)
  4025ef:	48 b8 72 69 6e 67 20 	movabs $0x6e6f6320676e6972,%rax
  4025f6:	63 6f 6e 
  4025f9:	48 89 43 10          	mov    %rax,0x10(%rbx)
  4025fd:	48 b8 74 61 69 6e 73 	movabs $0x6e6120736e696174,%rax
  402604:	20 61 6e 
  402607:	48 89 43 18          	mov    %rax,0x18(%rbx)
  40260b:	48 b8 20 69 6c 6c 65 	movabs $0x6c6167656c6c6920,%rax
  402612:	67 61 6c 
  402615:	48 89 43 20          	mov    %rax,0x20(%rbx)
  402619:	48 b8 20 6f 72 20 75 	movabs $0x72706e7520726f20,%rax
  402620:	6e 70 72 
  402623:	48 89 43 28          	mov    %rax,0x28(%rbx)
  402627:	48 b8 69 6e 74 61 62 	movabs $0x20656c6261746e69,%rax
  40262e:	6c 65 20 
  402631:	48 89 43 30          	mov    %rax,0x30(%rbx)
  402635:	48 b8 63 68 61 72 61 	movabs $0x6574636172616863,%rax
  40263c:	63 74 65 
  40263f:	48 89 43 38          	mov    %rax,0x38(%rbx)
  402643:	66 c7 43 40 72 2e    	movw   $0x2e72,0x40(%rbx)
  402649:	c6 43 42 00          	movb   $0x0,0x42(%rbx)
  40264d:	89 ef                	mov    %ebp,%edi
  40264f:	e8 5c e6 ff ff       	callq  400cb0 <close@plt>
  402654:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  402659:	e9 c2 03 00 00       	jmpq   402a20 <submitr+0x709>
  40265e:	4d 89 e1             	mov    %r12,%r9
  402661:	4c 8d 84 24 20 40 00 	lea    0x4020(%rsp),%r8
  402668:	00 
  402669:	4c 89 f9             	mov    %r15,%rcx
  40266c:	4c 89 f2             	mov    %r14,%rdx
  40266f:	be f8 44 40 00       	mov    $0x4044f8,%esi
  402674:	48 8d bc 24 20 60 00 	lea    0x6020(%rsp),%rdi
  40267b:	00 
  40267c:	b8 00 00 00 00       	mov    $0x0,%eax
  402681:	e8 5a e7 ff ff       	callq  400de0 <sprintf@plt>
  402686:	48 8d bc 24 20 60 00 	lea    0x6020(%rsp),%rdi
  40268d:	00 
  40268e:	b8 00 00 00 00       	mov    $0x0,%eax
  402693:	48 c7 c1 ff ff ff ff 	mov    $0xffffffffffffffff,%rcx
  40269a:	f2 ae                	repnz scas %es:(%rdi),%al
  40269c:	48 f7 d1             	not    %rcx
  40269f:	48 8d 51 ff          	lea    -0x1(%rcx),%rdx
  4026a3:	48 8d b4 24 20 60 00 	lea    0x6020(%rsp),%rsi
  4026aa:	00 
  4026ab:	89 ef                	mov    %ebp,%edi
  4026ad:	e8 ff fa ff ff       	callq  4021b1 <rio_writen>
  4026b2:	48 85 c0             	test   %rax,%rax
  4026b5:	79 6e                	jns    402725 <submitr+0x40e>
  4026b7:	48 b8 45 72 72 6f 72 	movabs $0x43203a726f727245,%rax
  4026be:	3a 20 43 
  4026c1:	48 89 03             	mov    %rax,(%rbx)
  4026c4:	48 b8 6c 69 65 6e 74 	movabs $0x6e7520746e65696c,%rax
  4026cb:	20 75 6e 
  4026ce:	48 89 43 08          	mov    %rax,0x8(%rbx)
  4026d2:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  4026d9:	74 6f 20 
  4026dc:	48 89 43 10          	mov    %rax,0x10(%rbx)
  4026e0:	48 b8 77 72 69 74 65 	movabs $0x6f74206574697277,%rax
  4026e7:	20 74 6f 
  4026ea:	48 89 43 18          	mov    %rax,0x18(%rbx)
  4026ee:	48 b8 20 74 68 65 20 	movabs $0x7365722065687420,%rax
  4026f5:	72 65 73 
  4026f8:	48 89 43 20          	mov    %rax,0x20(%rbx)
  4026fc:	48 b8 75 6c 74 20 73 	movabs $0x7672657320746c75,%rax
  402703:	65 72 76 
  402706:	48 89 43 28          	mov    %rax,0x28(%rbx)
  40270a:	66 c7 43 30 65 72    	movw   $0x7265,0x30(%rbx)
  402710:	c6 43 32 00          	movb   $0x0,0x32(%rbx)
  402714:	89 ef                	mov    %ebp,%edi
  402716:	e8 95 e5 ff ff       	callq  400cb0 <close@plt>
  40271b:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  402720:	e9 fb 02 00 00       	jmpq   402a20 <submitr+0x709>
  402725:	89 ee                	mov    %ebp,%esi
  402727:	48 8d bc 24 20 80 00 	lea    0x8020(%rsp),%rdi
  40272e:	00 
  40272f:	e8 6c f9 ff ff       	callq  4020a0 <rio_readinitb>
  402734:	ba 00 20 00 00       	mov    $0x2000,%edx
  402739:	48 8d b4 24 20 60 00 	lea    0x6020(%rsp),%rsi
  402740:	00 
  402741:	48 8d bc 24 20 80 00 	lea    0x8020(%rsp),%rdi
  402748:	00 
  402749:	e8 46 fb ff ff       	callq  402294 <rio_readlineb>
  40274e:	48 85 c0             	test   %rax,%rax
  402751:	7f 7d                	jg     4027d0 <submitr+0x4b9>
  402753:	48 b8 45 72 72 6f 72 	movabs $0x43203a726f727245,%rax
  40275a:	3a 20 43 
  40275d:	48 89 03             	mov    %rax,(%rbx)
  402760:	48 b8 6c 69 65 6e 74 	movabs $0x6e7520746e65696c,%rax
  402767:	20 75 6e 
  40276a:	48 89 43 08          	mov    %rax,0x8(%rbx)
  40276e:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  402775:	74 6f 20 
  402778:	48 89 43 10          	mov    %rax,0x10(%rbx)
  40277c:	48 b8 72 65 61 64 20 	movabs $0x7269662064616572,%rax
  402783:	66 69 72 
  402786:	48 89 43 18          	mov    %rax,0x18(%rbx)
  40278a:	48 b8 73 74 20 68 65 	movabs $0x6564616568207473,%rax
  402791:	61 64 65 
  402794:	48 89 43 20          	mov    %rax,0x20(%rbx)
  402798:	48 b8 72 20 66 72 6f 	movabs $0x72206d6f72662072,%rax
  40279f:	6d 20 72 
  4027a2:	48 89 43 28          	mov    %rax,0x28(%rbx)
  4027a6:	48 b8 65 73 75 6c 74 	movabs $0x657320746c757365,%rax
  4027ad:	20 73 65 
  4027b0:	48 89 43 30          	mov    %rax,0x30(%rbx)
  4027b4:	c7 43 38 72 76 65 72 	movl   $0x72657672,0x38(%rbx)
  4027bb:	c6 43 3c 00          	movb   $0x0,0x3c(%rbx)
  4027bf:	89 ef                	mov    %ebp,%edi
  4027c1:	e8 ea e4 ff ff       	callq  400cb0 <close@plt>
  4027c6:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  4027cb:	e9 50 02 00 00       	jmpq   402a20 <submitr+0x709>
  4027d0:	4c 8d 44 24 10       	lea    0x10(%rsp),%r8
  4027d5:	48 8d 8c 24 1c 20 00 	lea    0x201c(%rsp),%rcx
  4027dc:	00 
  4027dd:	48 8d 94 24 20 20 00 	lea    0x2020(%rsp),%rdx
  4027e4:	00 
  4027e5:	be 6f 45 40 00       	mov    $0x40456f,%esi
  4027ea:	48 8d bc 24 20 60 00 	lea    0x6020(%rsp),%rdi
  4027f1:	00 
  4027f2:	b8 00 00 00 00       	mov    $0x0,%eax
  4027f7:	e8 74 e5 ff ff       	callq  400d70 <__isoc99_sscanf@plt>
  4027fc:	e9 98 00 00 00       	jmpq   402899 <submitr+0x582>
  402801:	ba 00 20 00 00       	mov    $0x2000,%edx
  402806:	48 8d b4 24 20 60 00 	lea    0x6020(%rsp),%rsi
  40280d:	00 
  40280e:	48 8d bc 24 20 80 00 	lea    0x8020(%rsp),%rdi
  402815:	00 
  402816:	e8 79 fa ff ff       	callq  402294 <rio_readlineb>
  40281b:	48 85 c0             	test   %rax,%rax
  40281e:	7f 79                	jg     402899 <submitr+0x582>
  402820:	48 b8 45 72 72 6f 72 	movabs $0x43203a726f727245,%rax
  402827:	3a 20 43 
  40282a:	48 89 03             	mov    %rax,(%rbx)
  40282d:	48 b8 6c 69 65 6e 74 	movabs $0x6e7520746e65696c,%rax
  402834:	20 75 6e 
  402837:	48 89 43 08          	mov    %rax,0x8(%rbx)
  40283b:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  402842:	74 6f 20 
  402845:	48 89 43 10          	mov    %rax,0x10(%rbx)
  402849:	48 b8 72 65 61 64 20 	movabs $0x6165682064616572,%rax
  402850:	68 65 61 
  402853:	48 89 43 18          	mov    %rax,0x18(%rbx)
  402857:	48 b8 64 65 72 73 20 	movabs $0x6f72662073726564,%rax
  40285e:	66 72 6f 
  402861:	48 89 43 20          	mov    %rax,0x20(%rbx)
  402865:	48 b8 6d 20 74 68 65 	movabs $0x657220656874206d,%rax
  40286c:	20 72 65 
  40286f:	48 89 43 28          	mov    %rax,0x28(%rbx)
  402873:	48 b8 73 75 6c 74 20 	movabs $0x72657320746c7573,%rax
  40287a:	73 65 72 
  40287d:	48 89 43 30          	mov    %rax,0x30(%rbx)
  402881:	c7 43 38 76 65 72 00 	movl   $0x726576,0x38(%rbx)
  402888:	89 ef                	mov    %ebp,%edi
  40288a:	e8 21 e4 ff ff       	callq  400cb0 <close@plt>
  40288f:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  402894:	e9 87 01 00 00       	jmpq   402a20 <submitr+0x709>
  402899:	0f b6 84 24 20 60 00 	movzbl 0x6020(%rsp),%eax
  4028a0:	00 
  4028a1:	83 e8 0d             	sub    $0xd,%eax
  4028a4:	75 15                	jne    4028bb <submitr+0x5a4>
  4028a6:	0f b6 84 24 21 60 00 	movzbl 0x6021(%rsp),%eax
  4028ad:	00 
  4028ae:	83 e8 0a             	sub    $0xa,%eax
  4028b1:	75 08                	jne    4028bb <submitr+0x5a4>
  4028b3:	0f b6 84 24 22 60 00 	movzbl 0x6022(%rsp),%eax
  4028ba:	00 
  4028bb:	85 c0                	test   %eax,%eax
  4028bd:	0f 85 3e ff ff ff    	jne    402801 <submitr+0x4ea>
  4028c3:	ba 00 20 00 00       	mov    $0x2000,%edx
  4028c8:	48 8d b4 24 20 60 00 	lea    0x6020(%rsp),%rsi
  4028cf:	00 
  4028d0:	48 8d bc 24 20 80 00 	lea    0x8020(%rsp),%rdi
  4028d7:	00 
  4028d8:	e8 b7 f9 ff ff       	callq  402294 <rio_readlineb>
  4028dd:	48 85 c0             	test   %rax,%rax
  4028e0:	0f 8f 83 00 00 00    	jg     402969 <submitr+0x652>
  4028e6:	48 b8 45 72 72 6f 72 	movabs $0x43203a726f727245,%rax
  4028ed:	3a 20 43 
  4028f0:	48 89 03             	mov    %rax,(%rbx)
  4028f3:	48 b8 6c 69 65 6e 74 	movabs $0x6e7520746e65696c,%rax
  4028fa:	20 75 6e 
  4028fd:	48 89 43 08          	mov    %rax,0x8(%rbx)
  402901:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  402908:	74 6f 20 
  40290b:	48 89 43 10          	mov    %rax,0x10(%rbx)
  40290f:	48 b8 72 65 61 64 20 	movabs $0x6174732064616572,%rax
  402916:	73 74 61 
  402919:	48 89 43 18          	mov    %rax,0x18(%rbx)
  40291d:	48 b8 74 75 73 20 6d 	movabs $0x7373656d20737574,%rax
  402924:	65 73 73 
  402927:	48 89 43 20          	mov    %rax,0x20(%rbx)
  40292b:	48 b8 61 67 65 20 66 	movabs $0x6d6f726620656761,%rax
  402932:	72 6f 6d 
  402935:	48 89 43 28          	mov    %rax,0x28(%rbx)
  402939:	48 b8 20 72 65 73 75 	movabs $0x20746c7573657220,%rax
  402940:	6c 74 20 
  402943:	48 89 43 30          	mov    %rax,0x30(%rbx)
  402947:	c7 43 38 73 65 72 76 	movl   $0x76726573,0x38(%rbx)
  40294e:	66 c7 43 3c 65 72    	movw   $0x7265,0x3c(%rbx)
  402954:	c6 43 3e 00          	movb   $0x0,0x3e(%rbx)
  402958:	89 ef                	mov    %ebp,%edi
  40295a:	e8 51 e3 ff ff       	callq  400cb0 <close@plt>
  40295f:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  402964:	e9 b7 00 00 00       	jmpq   402a20 <submitr+0x709>
  402969:	8b 94 24 1c 20 00 00 	mov    0x201c(%rsp),%edx
  402970:	81 fa c8 00 00 00    	cmp    $0xc8,%edx
  402976:	74 28                	je     4029a0 <submitr+0x689>
  402978:	48 8d 4c 24 10       	lea    0x10(%rsp),%rcx
  40297d:	be 38 45 40 00       	mov    $0x404538,%esi
  402982:	48 89 df             	mov    %rbx,%rdi
  402985:	b8 00 00 00 00       	mov    $0x0,%eax
  40298a:	e8 51 e4 ff ff       	callq  400de0 <sprintf@plt>
  40298f:	89 ef                	mov    %ebp,%edi
  402991:	e8 1a e3 ff ff       	callq  400cb0 <close@plt>
  402996:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  40299b:	e9 80 00 00 00       	jmpq   402a20 <submitr+0x709>
  4029a0:	48 8d b4 24 20 60 00 	lea    0x6020(%rsp),%rsi
  4029a7:	00 
  4029a8:	48 89 df             	mov    %rbx,%rdi
  4029ab:	e8 90 e2 ff ff       	callq  400c40 <strcpy@plt>
  4029b0:	89 ef                	mov    %ebp,%edi
  4029b2:	e8 f9 e2 ff ff       	callq  400cb0 <close@plt>
  4029b7:	0f b6 03             	movzbl (%rbx),%eax
  4029ba:	83 e8 4f             	sub    $0x4f,%eax
  4029bd:	75 18                	jne    4029d7 <submitr+0x6c0>
  4029bf:	0f b6 53 01          	movzbl 0x1(%rbx),%edx
  4029c3:	83 ea 4b             	sub    $0x4b,%edx
  4029c6:	75 11                	jne    4029d9 <submitr+0x6c2>
  4029c8:	0f b6 53 02          	movzbl 0x2(%rbx),%edx
  4029cc:	83 ea 0a             	sub    $0xa,%edx
  4029cf:	75 08                	jne    4029d9 <submitr+0x6c2>
  4029d1:	0f b6 53 03          	movzbl 0x3(%rbx),%edx
  4029d5:	eb 02                	jmp    4029d9 <submitr+0x6c2>
  4029d7:	89 c2                	mov    %eax,%edx
  4029d9:	85 d2                	test   %edx,%edx
  4029db:	74 30                	je     402a0d <submitr+0x6f6>
  4029dd:	bf 80 45 40 00       	mov    $0x404580,%edi
  4029e2:	b9 05 00 00 00       	mov    $0x5,%ecx
  4029e7:	48 89 de             	mov    %rbx,%rsi
  4029ea:	f3 a6                	repz cmpsb %es:(%rdi),%ds:(%rsi)
  4029ec:	0f 97 c1             	seta   %cl
  4029ef:	0f 92 c2             	setb   %dl
  4029f2:	38 d1                	cmp    %dl,%cl
  4029f4:	74 1e                	je     402a14 <submitr+0x6fd>
  4029f6:	85 c0                	test   %eax,%eax
  4029f8:	75 0d                	jne    402a07 <submitr+0x6f0>
  4029fa:	0f b6 43 01          	movzbl 0x1(%rbx),%eax
  4029fe:	83 e8 4b             	sub    $0x4b,%eax
  402a01:	75 04                	jne    402a07 <submitr+0x6f0>
  402a03:	0f b6 43 02          	movzbl 0x2(%rbx),%eax
  402a07:	85 c0                	test   %eax,%eax
  402a09:	75 10                	jne    402a1b <submitr+0x704>
  402a0b:	eb 13                	jmp    402a20 <submitr+0x709>
  402a0d:	b8 00 00 00 00       	mov    $0x0,%eax
  402a12:	eb 0c                	jmp    402a20 <submitr+0x709>
  402a14:	b8 00 00 00 00       	mov    $0x0,%eax
  402a19:	eb 05                	jmp    402a20 <submitr+0x709>
  402a1b:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  402a20:	48 81 c4 48 a0 00 00 	add    $0xa048,%rsp
  402a27:	5b                   	pop    %rbx
  402a28:	5d                   	pop    %rbp
  402a29:	41 5c                	pop    %r12
  402a2b:	41 5d                	pop    %r13
  402a2d:	41 5e                	pop    %r14
  402a2f:	41 5f                	pop    %r15
  402a31:	c3                   	retq   

0000000000402a32 <init_timeout>:
  402a32:	53                   	push   %rbx
  402a33:	89 fb                	mov    %edi,%ebx
  402a35:	85 ff                	test   %edi,%edi
  402a37:	74 1f                	je     402a58 <init_timeout+0x26>
  402a39:	85 ff                	test   %edi,%edi
  402a3b:	79 05                	jns    402a42 <init_timeout+0x10>
  402a3d:	bb 00 00 00 00       	mov    $0x0,%ebx
  402a42:	be b2 20 40 00       	mov    $0x4020b2,%esi
  402a47:	bf 0e 00 00 00       	mov    $0xe,%edi
  402a4c:	e8 8f e2 ff ff       	callq  400ce0 <signal@plt>
  402a51:	89 df                	mov    %ebx,%edi
  402a53:	e8 48 e2 ff ff       	callq  400ca0 <alarm@plt>
  402a58:	5b                   	pop    %rbx
  402a59:	c3                   	retq   

0000000000402a5a <init_driver>:
  402a5a:	55                   	push   %rbp
  402a5b:	53                   	push   %rbx
  402a5c:	48 83 ec 18          	sub    $0x18,%rsp
  402a60:	48 89 fd             	mov    %rdi,%rbp
  402a63:	be 01 00 00 00       	mov    $0x1,%esi
  402a68:	bf 0d 00 00 00       	mov    $0xd,%edi
  402a6d:	e8 6e e2 ff ff       	callq  400ce0 <signal@plt>
  402a72:	be 01 00 00 00       	mov    $0x1,%esi
  402a77:	bf 1d 00 00 00       	mov    $0x1d,%edi
  402a7c:	e8 5f e2 ff ff       	callq  400ce0 <signal@plt>
  402a81:	be 01 00 00 00       	mov    $0x1,%esi
  402a86:	bf 1d 00 00 00       	mov    $0x1d,%edi
  402a8b:	e8 50 e2 ff ff       	callq  400ce0 <signal@plt>
  402a90:	ba 00 00 00 00       	mov    $0x0,%edx
  402a95:	be 01 00 00 00       	mov    $0x1,%esi
  402a9a:	bf 02 00 00 00       	mov    $0x2,%edi
  402a9f:	e8 6c e3 ff ff       	callq  400e10 <socket@plt>
  402aa4:	89 c3                	mov    %eax,%ebx
  402aa6:	85 c0                	test   %eax,%eax
  402aa8:	79 4f                	jns    402af9 <init_driver+0x9f>
  402aaa:	48 b8 45 72 72 6f 72 	movabs $0x43203a726f727245,%rax
  402ab1:	3a 20 43 
  402ab4:	48 89 45 00          	mov    %rax,0x0(%rbp)
  402ab8:	48 b8 6c 69 65 6e 74 	movabs $0x6e7520746e65696c,%rax
  402abf:	20 75 6e 
  402ac2:	48 89 45 08          	mov    %rax,0x8(%rbp)
  402ac6:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  402acd:	74 6f 20 
  402ad0:	48 89 45 10          	mov    %rax,0x10(%rbp)
  402ad4:	48 b8 63 72 65 61 74 	movabs $0x7320657461657263,%rax
  402adb:	65 20 73 
  402ade:	48 89 45 18          	mov    %rax,0x18(%rbp)
  402ae2:	c7 45 20 6f 63 6b 65 	movl   $0x656b636f,0x20(%rbp)
  402ae9:	66 c7 45 24 74 00    	movw   $0x74,0x24(%rbp)
  402aef:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  402af4:	e9 23 01 00 00       	jmpq   402c1c <init_driver+0x1c2>
  402af9:	bf c2 30 40 00       	mov    $0x4030c2,%edi
  402afe:	e8 ed e1 ff ff       	callq  400cf0 <gethostbyname@plt>
  402b03:	48 85 c0             	test   %rax,%rax
  402b06:	75 68                	jne    402b70 <init_driver+0x116>
  402b08:	48 b8 45 72 72 6f 72 	movabs $0x44203a726f727245,%rax
  402b0f:	3a 20 44 
  402b12:	48 89 45 00          	mov    %rax,0x0(%rbp)
  402b16:	48 b8 4e 53 20 69 73 	movabs $0x6e7520736920534e,%rax
  402b1d:	20 75 6e 
  402b20:	48 89 45 08          	mov    %rax,0x8(%rbp)
  402b24:	48 b8 61 62 6c 65 20 	movabs $0x206f7420656c6261,%rax
  402b2b:	74 6f 20 
  402b2e:	48 89 45 10          	mov    %rax,0x10(%rbp)
  402b32:	48 b8 72 65 73 6f 6c 	movabs $0x2065766c6f736572,%rax
  402b39:	76 65 20 
  402b3c:	48 89 45 18          	mov    %rax,0x18(%rbp)
  402b40:	48 b8 73 65 72 76 65 	movabs $0x6120726576726573,%rax
  402b47:	72 20 61 
  402b4a:	48 89 45 20          	mov    %rax,0x20(%rbp)
  402b4e:	c7 45 28 64 64 72 65 	movl   $0x65726464,0x28(%rbp)
  402b55:	66 c7 45 2c 73 73    	movw   $0x7373,0x2c(%rbp)
  402b5b:	c6 45 2e 00          	movb   $0x0,0x2e(%rbp)
  402b5f:	89 df                	mov    %ebx,%edi
  402b61:	e8 4a e1 ff ff       	callq  400cb0 <close@plt>
  402b66:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  402b6b:	e9 ac 00 00 00       	jmpq   402c1c <init_driver+0x1c2>
  402b70:	48 c7 04 24 00 00 00 	movq   $0x0,(%rsp)
  402b77:	00 
  402b78:	48 c7 44 24 08 00 00 	movq   $0x0,0x8(%rsp)
  402b7f:	00 00 
  402b81:	66 c7 04 24 02 00    	movw   $0x2,(%rsp)
  402b87:	48 8b 48 18          	mov    0x18(%rax),%rcx
  402b8b:	48 63 50 14          	movslq 0x14(%rax),%rdx
  402b8f:	48 8d 74 24 04       	lea    0x4(%rsp),%rsi
  402b94:	48 8b 39             	mov    (%rcx),%rdi
  402b97:	e8 f4 e1 ff ff       	callq  400d90 <bcopy@plt>
  402b9c:	66 c7 44 24 02 ea 62 	movw   $0x62ea,0x2(%rsp)
  402ba3:	ba 10 00 00 00       	mov    $0x10,%edx
  402ba8:	48 89 e6             	mov    %rsp,%rsi
  402bab:	89 df                	mov    %ebx,%edi
  402bad:	e8 4e e2 ff ff       	callq  400e00 <connect@plt>
  402bb2:	85 c0                	test   %eax,%eax
  402bb4:	79 50                	jns    402c06 <init_driver+0x1ac>
  402bb6:	48 b8 45 72 72 6f 72 	movabs $0x55203a726f727245,%rax
  402bbd:	3a 20 55 
  402bc0:	48 89 45 00          	mov    %rax,0x0(%rbp)
  402bc4:	48 b8 6e 61 62 6c 65 	movabs $0x6f7420656c62616e,%rax
  402bcb:	20 74 6f 
  402bce:	48 89 45 08          	mov    %rax,0x8(%rbp)
  402bd2:	48 b8 20 63 6f 6e 6e 	movabs $0x7463656e6e6f6320,%rax
  402bd9:	65 63 74 
  402bdc:	48 89 45 10          	mov    %rax,0x10(%rbp)
  402be0:	48 b8 20 74 6f 20 73 	movabs $0x76726573206f7420,%rax
  402be7:	65 72 76 
  402bea:	48 89 45 18          	mov    %rax,0x18(%rbp)
  402bee:	66 c7 45 20 65 72    	movw   $0x7265,0x20(%rbp)
  402bf4:	c6 45 22 00          	movb   $0x0,0x22(%rbp)
  402bf8:	89 df                	mov    %ebx,%edi
  402bfa:	e8 b1 e0 ff ff       	callq  400cb0 <close@plt>
  402bff:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  402c04:	eb 16                	jmp    402c1c <init_driver+0x1c2>
  402c06:	89 df                	mov    %ebx,%edi
  402c08:	e8 a3 e0 ff ff       	callq  400cb0 <close@plt>
  402c0d:	66 c7 45 00 4f 4b    	movw   $0x4b4f,0x0(%rbp)
  402c13:	c6 45 02 00          	movb   $0x0,0x2(%rbp)
  402c17:	b8 00 00 00 00       	mov    $0x0,%eax
  402c1c:	48 83 c4 18          	add    $0x18,%rsp
  402c20:	5b                   	pop    %rbx
  402c21:	5d                   	pop    %rbp
  402c22:	c3                   	retq   

0000000000402c23 <driver_post>:
  402c23:	53                   	push   %rbx
  402c24:	48 83 ec 10          	sub    $0x10,%rsp
  402c28:	4c 89 cb             	mov    %r9,%rbx
  402c2b:	45 85 c0             	test   %r8d,%r8d
  402c2e:	74 22                	je     402c52 <driver_post+0x2f>
  402c30:	48 89 ce             	mov    %rcx,%rsi
  402c33:	bf 85 45 40 00       	mov    $0x404585,%edi
  402c38:	b8 00 00 00 00       	mov    $0x0,%eax
  402c3d:	e8 3e e0 ff ff       	callq  400c80 <printf@plt>
  402c42:	66 c7 03 4f 4b       	movw   $0x4b4f,(%rbx)
  402c47:	c6 43 02 00          	movb   $0x0,0x2(%rbx)
  402c4b:	b8 00 00 00 00       	mov    $0x0,%eax
  402c50:	eb 39                	jmp    402c8b <driver_post+0x68>
  402c52:	48 85 ff             	test   %rdi,%rdi
  402c55:	74 26                	je     402c7d <driver_post+0x5a>
  402c57:	80 3f 00             	cmpb   $0x0,(%rdi)
  402c5a:	74 21                	je     402c7d <driver_post+0x5a>
  402c5c:	4c 89 0c 24          	mov    %r9,(%rsp)
  402c60:	49 89 c9             	mov    %rcx,%r9
  402c63:	49 89 d0             	mov    %rdx,%r8
  402c66:	48 89 f9             	mov    %rdi,%rcx
  402c69:	48 89 f2             	mov    %rsi,%rdx
  402c6c:	be 62 ea 00 00       	mov    $0xea62,%esi
  402c71:	bf c2 30 40 00       	mov    $0x4030c2,%edi
  402c76:	e8 9c f6 ff ff       	callq  402317 <submitr>
  402c7b:	eb 0e                	jmp    402c8b <driver_post+0x68>
  402c7d:	66 c7 03 4f 4b       	movw   $0x4b4f,(%rbx)
  402c82:	c6 43 02 00          	movb   $0x0,0x2(%rbx)
  402c86:	b8 00 00 00 00       	mov    $0x0,%eax
  402c8b:	48 83 c4 10          	add    $0x10,%rsp
  402c8f:	5b                   	pop    %rbx
  402c90:	c3                   	retq   

0000000000402c91 <check>:
  402c91:	89 f8                	mov    %edi,%eax
  402c93:	c1 e8 1c             	shr    $0x1c,%eax
  402c96:	85 c0                	test   %eax,%eax
  402c98:	74 1d                	je     402cb7 <check+0x26>
  402c9a:	b9 00 00 00 00       	mov    $0x0,%ecx
  402c9f:	eb 0b                	jmp    402cac <check+0x1b>
  402ca1:	89 f8                	mov    %edi,%eax
  402ca3:	d3 e8                	shr    %cl,%eax
  402ca5:	3c 0a                	cmp    $0xa,%al
  402ca7:	74 14                	je     402cbd <check+0x2c>
  402ca9:	83 c1 08             	add    $0x8,%ecx
  402cac:	83 f9 1f             	cmp    $0x1f,%ecx
  402caf:	7e f0                	jle    402ca1 <check+0x10>
  402cb1:	b8 01 00 00 00       	mov    $0x1,%eax
  402cb6:	c3                   	retq   
  402cb7:	b8 00 00 00 00       	mov    $0x0,%eax
  402cbc:	c3                   	retq   
  402cbd:	b8 00 00 00 00       	mov    $0x0,%eax
  402cc2:	c3                   	retq   

0000000000402cc3 <gencookie>:
  402cc3:	53                   	push   %rbx
  402cc4:	83 c7 01             	add    $0x1,%edi
  402cc7:	e8 54 df ff ff       	callq  400c20 <srandom@plt>
  402ccc:	e8 7f e0 ff ff       	callq  400d50 <random@plt>
  402cd1:	89 c3                	mov    %eax,%ebx
  402cd3:	89 c7                	mov    %eax,%edi
  402cd5:	e8 b7 ff ff ff       	callq  402c91 <check>
  402cda:	85 c0                	test   %eax,%eax
  402cdc:	74 ee                	je     402ccc <gencookie+0x9>
  402cde:	89 d8                	mov    %ebx,%eax
  402ce0:	5b                   	pop    %rbx
  402ce1:	c3                   	retq   
  402ce2:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  402ce9:	00 00 00 
  402cec:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000402cf0 <__libc_csu_init>:
  402cf0:	41 57                	push   %r15
  402cf2:	41 89 ff             	mov    %edi,%r15d
  402cf5:	41 56                	push   %r14
  402cf7:	49 89 f6             	mov    %rsi,%r14
  402cfa:	41 55                	push   %r13
  402cfc:	49 89 d5             	mov    %rdx,%r13
  402cff:	41 54                	push   %r12
  402d01:	4c 8d 25 08 31 20 00 	lea    0x203108(%rip),%r12        # 605e10 <__frame_dummy_init_array_entry>
  402d08:	55                   	push   %rbp
  402d09:	48 8d 2d 08 31 20 00 	lea    0x203108(%rip),%rbp        # 605e18 <__init_array_end>
  402d10:	53                   	push   %rbx
  402d11:	4c 29 e5             	sub    %r12,%rbp
  402d14:	31 db                	xor    %ebx,%ebx
  402d16:	48 c1 fd 03          	sar    $0x3,%rbp
  402d1a:	48 83 ec 08          	sub    $0x8,%rsp
  402d1e:	e8 a5 de ff ff       	callq  400bc8 <_init>
  402d23:	48 85 ed             	test   %rbp,%rbp
  402d26:	74 1e                	je     402d46 <__libc_csu_init+0x56>
  402d28:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  402d2f:	00 
  402d30:	4c 89 ea             	mov    %r13,%rdx
  402d33:	4c 89 f6             	mov    %r14,%rsi
  402d36:	44 89 ff             	mov    %r15d,%edi
  402d39:	41 ff 14 dc          	callq  *(%r12,%rbx,8)
  402d3d:	48 83 c3 01          	add    $0x1,%rbx
  402d41:	48 39 eb             	cmp    %rbp,%rbx
  402d44:	75 ea                	jne    402d30 <__libc_csu_init+0x40>
  402d46:	48 83 c4 08          	add    $0x8,%rsp
  402d4a:	5b                   	pop    %rbx
  402d4b:	5d                   	pop    %rbp
  402d4c:	41 5c                	pop    %r12
  402d4e:	41 5d                	pop    %r13
  402d50:	41 5e                	pop    %r14
  402d52:	41 5f                	pop    %r15
  402d54:	c3                   	retq   
  402d55:	90                   	nop
  402d56:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  402d5d:	00 00 00 

0000000000402d60 <__libc_csu_fini>:
  402d60:	f3 c3                	repz retq 

Disassembly of section .fini:

0000000000402d64 <_fini>:
  402d64:	48 83 ec 08          	sub    $0x8,%rsp
  402d68:	48 83 c4 08          	add    $0x8,%rsp
  402d6c:	c3                   	retq   
