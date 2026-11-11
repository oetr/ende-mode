(root ".")
(group read-only project_info file_info list_project_files read_file search_project)
(group write-only edit_file create_file apply_patch)

(resource project 
	  (include "*.el")
	  (include "websocket.txt")
	  (include "add-header.sh")
	  (include "Makefile")
	  (include "Containerfile")
	  (include "LICENSE")
	  (exclude ".ende/*"))

(tools read-only write-only)
(set-tool-resources read-only write-only :resources project)

(tools run_host_command)
(restrict-host-commands 
 "git"
 "emacs"
 (:prefix-args "make" "check")
 (:prefix-args "make" "test")
 )

(tools spawn_agent)

(agent-config worker
	      (root ".")
	      (tools read-only  write-only)
	      (set-tool-resources read-only write-only 
				  :resources project)

	      (tools run_host_command)
	      (restrict-host-commands 
	       "git"
	       "emacs"
	       (:prefix-args "make" "check")
	       (:prefix-args "make" "test")
	       )

	      (resource internet :type url (include "*"))
	      (tools web_fetch)
	      (set-tool-resources web_fetch :resources internet)
	      )

(spawn-agent-config worker)

;;(tools generate_image)

;; (tools web_fetch)
;; (set-tool-resources web_fetch :resources internet)

;; (resource internet :type url
;;   (include "*")
;;   (include "http://*/**"))

;;(podman-image "localhost/ende-dev@sha256:1c646a001698c828cccd934fc1369c650d9f93890a5dd6d62c85009e87e09c9a")
;;(set-tool-resources run_sandbox_command :resources project)
;;(tools run_sandbox_command)

;;(resource sandbox-out (include "sandbox-copy/**"))
;;(set-tool-resources copy_from_sandbox :resources sandbox-out)
