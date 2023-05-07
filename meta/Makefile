checks:
	cd ~/git/melpa/ && make clean && rm -rf packages/devil* working/devil/
	cd ~/git/melpa/ && make recipes/devil
	-cd ~/git/melpa/ && STABLE=t make recipes/devil
	cd ~/git/melpa/ && make sandbox INSTALL=devil
	cd ~/git/melpa/ && ls -l packages/ packages-stable/ sandbox/elpa/

smiley:
	emacs -q -l smiley.el example.el example.md
