(pushnew (uiop:getcwd) ql:*local-project-directories*)
(ql:quickload :pyrice)
(asdf:make :pyrice)
