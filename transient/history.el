((magit-bisect nil)
 (magit-blame
  ("-w"))
 (magit-branch nil)
 (magit-commit nil)
 (magit-diff
  ("--no-ext-diff" "--stat"))
 (magit-dispatch nil)
 (magit-fetch nil)
 (magit-file-dispatch nil)
 (magit-gitignore nil)
 (magit-log
  ("-n256" "--graph" "--decorate")
  ("-Simport"
   ("--" "components/base/network/gce-mixin.libsonnet"))
  ("-Sproject"
   ("--" "components/cluster/config.libsonnet"))
  ("-n256" "-Smonitoring-services" "--graph" "--decorate"))
 (magit-log:-S "import" "project" "monitoring-services")
 (magit-merge nil)
 (magit-pull nil
             ("--rebase"))
 (magit-push nil
             ("--force-with-lease"))
 (magit-rebase nil)
 (magit-remote
  ("-f"))
 (magit-run nil)
 (magit-stash nil)
 (magit-submodule nil
                  ("--recursive"))
 (magit-tag nil))
