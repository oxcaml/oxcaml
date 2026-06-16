  $ echo "S .\nB .\nFLG -nopervasives" > .merlin
  $ $MERLIN single outline < foo.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 91,
          "col": 0
        },
        "end": {
          "line": 97,
          "col": 3
        },
        "name": "final_let",
        "kind": "Value",
        "type": "< foo : int >",
        "children": [
          {
            "start": {
              "line": 92,
              "col": 2
            },
            "end": {
              "line": 95,
              "col": 7
            },
            "name": "c",
            "kind": "Value",
            "type": "< foo : int >",
            "children": [
              {
                "start": {
                  "line": 94,
                  "col": 6
                },
                "end": {
                  "line": 94,
                  "col": 21
                },
                "name": "foo",
                "kind": "Method",
                "type": null,
                "children": [],
                "deprecated": false,
                "selection": {
                  "start": {
                    "line": 94,
                    "col": 13
                  },
                  "end": {
                    "line": 94,
                    "col": 16
                  }
                }
              }
            ],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 92,
                "col": 6
              },
              "end": {
                "line": 92,
                "col": 7
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 91,
            "col": 4
          },
          "end": {
            "line": 91,
            "col": 13
          }
        }
      },
      {
        "start": {
          "line": 89,
          "col": 0
        },
        "end": {
          "line": 89,
          "col": 19
        },
        "name": "tb",
        "kind": "ClassType",
        "type": null,
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 89,
            "col": 4
          },
          "end": {
            "line": 89,
            "col": 6
          }
        }
      },
      {
        "start": {
          "line": 85,
          "col": 0
        },
        "end": {
          "line": 87,
          "col": 3
        },
        "name": "ta",
        "kind": "ClassType",
        "type": null,
        "children": [
          {
            "start": {
              "line": 86,
              "col": 2
            },
            "end": {
              "line": 86,
              "col": 35
            },
            "name": "baz",
            "kind": "Method",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 86,
                "col": 2
              },
              "end": {
                "line": 86,
                "col": 35
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 85,
            "col": 11
          },
          "end": {
            "line": 85,
            "col": 13
          }
        }
      },
      {
        "start": {
          "line": 79,
          "col": 0
        },
        "end": {
          "line": 83,
          "col": 5
        },
        "name": "b",
        "kind": "Class",
        "type": null,
        "children": [
          {
            "start": {
              "line": 82,
              "col": 4
            },
            "end": {
              "line": 82,
              "col": 36
            },
            "name": "bar",
            "kind": "Method",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 82,
                "col": 11
              },
              "end": {
                "line": 82,
                "col": 14
              }
            }
          },
          {
            "start": {
              "line": 81,
              "col": 4
            },
            "end": {
              "line": 81,
              "col": 16
            },
            "name": "foo",
            "kind": "Value",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 81,
                "col": 8
              },
              "end": {
                "line": 81,
                "col": 11
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 79,
            "col": 4
          },
          "end": {
            "line": 79,
            "col": 5
          }
        }
      },
      {
        "start": {
          "line": 69,
          "col": 0
        },
        "end": {
          "line": 77,
          "col": 5
        },
        "name": "a",
        "kind": "Class",
        "type": null,
        "children": [
          {
            "start": {
              "line": 71,
              "col": 4
            },
            "end": {
              "line": 76,
              "col": 9
            },
            "name": "b",
            "kind": "Value",
            "type": null,
            "children": [
              {
                "start": {
                  "line": 73,
                  "col": 8
                },
                "end": {
                  "line": 75,
                  "col": 32
                },
                "name": "inside_a_b",
                "kind": "Method",
                "type": null,
                "children": [],
                "deprecated": false,
                "selection": {
                  "start": {
                    "line": 73,
                    "col": 15
                  },
                  "end": {
                    "line": 73,
                    "col": 25
                  }
                }
              }
            ],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 71,
                "col": 8
              },
              "end": {
                "line": 71,
                "col": 9
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 69,
            "col": 6
          },
          "end": {
            "line": 69,
            "col": 7
          }
        }
      },
      {
        "start": {
          "line": 67,
          "col": 0
        },
        "end": {
          "line": 67,
          "col": 18
        },
        "name": "c",
        "kind": "Class",
        "type": null,
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 67,
            "col": 4
          },
          "end": {
            "line": 67,
            "col": 5
          }
        }
      },
      {
        "start": {
          "line": 61,
          "col": 0
        },
        "end": {
          "line": 65,
          "col": 5
        },
        "name": "b",
        "kind": "Class",
        "type": null,
        "children": [
          {
            "start": {
              "line": 64,
              "col": 4
            },
            "end": {
              "line": 64,
              "col": 39
            },
            "name": "bar",
            "kind": "Method",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 64,
                "col": 11
              },
              "end": {
                "line": 64,
                "col": 14
              }
            }
          },
          {
            "start": {
              "line": 63,
              "col": 4
            },
            "end": {
              "line": 63,
              "col": 16
            },
            "name": "foo",
            "kind": "Value",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 63,
                "col": 8
              },
              "end": {
                "line": 63,
                "col": 11
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 61,
            "col": 6
          },
          "end": {
            "line": 61,
            "col": 7
          }
        }
      },
      {
        "start": {
          "line": 59,
          "col": 0
        },
        "end": {
          "line": 59,
          "col": 19
        },
        "name": "tb",
        "kind": "ClassType",
        "type": null,
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 59,
            "col": 4
          },
          "end": {
            "line": 59,
            "col": 6
          }
        }
      },
      {
        "start": {
          "line": 57,
          "col": 0
        },
        "end": {
          "line": 57,
          "col": 26
        },
        "name": "ta",
        "kind": "ClassType",
        "type": null,
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 57,
            "col": 11
          },
          "end": {
            "line": 57,
            "col": 13
          }
        }
      },
      {
        "start": {
          "line": 55,
          "col": 0
        },
        "end": {
          "line": 55,
          "col": 18
        },
        "name": "c",
        "kind": "Class",
        "type": null,
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 55,
            "col": 4
          },
          "end": {
            "line": 55,
            "col": 5
          }
        }
      },
      {
        "start": {
          "line": 53,
          "col": 0
        },
        "end": {
          "line": 53,
          "col": 18
        },
        "name": "b",
        "kind": "Class",
        "type": null,
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 53,
            "col": 4
          },
          "end": {
            "line": 53,
            "col": 5
          }
        }
      },
      {
        "start": {
          "line": 51,
          "col": 0
        },
        "end": {
          "line": 51,
          "col": 20
        },
        "name": "a",
        "kind": "Class",
        "type": null,
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 51,
            "col": 6
          },
          "end": {
            "line": 51,
            "col": 7
          }
        }
      },
      {
        "start": {
          "line": 40,
          "col": 0
        },
        "end": {
          "line": 49,
          "col": 3
        },
        "name": "S",
        "kind": "Signature",
        "type": null,
        "children": [
          {
            "start": {
              "line": 46,
              "col": 6
            },
            "end": {
              "line": 46,
              "col": 20
            },
            "name": "y",
            "kind": "Value",
            "type": "string",
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 46,
                "col": 10
              },
              "end": {
                "line": 46,
                "col": 11
              }
            }
          },
          {
            "start": {
              "line": 45,
              "col": 6
            },
            "end": {
              "line": 45,
              "col": 12
            },
            "name": "u",
            "kind": "Type",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 45,
                "col": 11
              },
              "end": {
                "line": 45,
                "col": 12
              }
            }
          },
          {
            "start": {
              "line": 43,
              "col": 4
            },
            "end": {
              "line": 43,
              "col": 15
            },
            "name": "x",
            "kind": "Value",
            "type": "int",
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 43,
                "col": 8
              },
              "end": {
                "line": 43,
                "col": 9
              }
            }
          },
          {
            "start": {
              "line": 42,
              "col": 4
            },
            "end": {
              "line": 42,
              "col": 10
            },
            "name": "t",
            "kind": "Type",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 42,
                "col": 9
              },
              "end": {
                "line": 42,
                "col": 10
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 40,
            "col": 12
          },
          "end": {
            "line": 40,
            "col": 13
          }
        }
      },
      {
        "start": {
          "line": 36,
          "col": 4
        },
        "end": {
          "line": 36,
          "col": 15
        },
        "name": "y",
        "kind": "Value",
        "type": "string",
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 36,
            "col": 8
          },
          "end": {
            "line": 36,
            "col": 9
          }
        }
      },
      {
        "start": {
          "line": 35,
          "col": 4
        },
        "end": {
          "line": 35,
          "col": 10
        },
        "name": "u",
        "kind": "Type",
        "type": null,
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 35,
            "col": 9
          },
          "end": {
            "line": 35,
            "col": 10
          }
        }
      },
      {
        "start": {
          "line": 33,
          "col": 2
        },
        "end": {
          "line": 33,
          "col": 11
        },
        "name": "x",
        "kind": "Value",
        "type": "int",
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 33,
            "col": 6
          },
          "end": {
            "line": 33,
            "col": 7
          }
        }
      },
      {
        "start": {
          "line": 32,
          "col": 2
        },
        "end": {
          "line": 32,
          "col": 8
        },
        "name": "t",
        "kind": "Type",
        "type": null,
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 32,
            "col": 7
          },
          "end": {
            "line": 32,
            "col": 8
          }
        }
      },
      {
        "start": {
          "line": 25,
          "col": 0
        },
        "end": {
          "line": 29,
          "col": 3
        },
        "name": "point",
        "kind": "Type",
        "type": null,
        "children": [
          {
            "start": {
              "line": 28,
              "col": 4
            },
            "end": {
              "line": 28,
              "col": 10
            },
            "name": "z",
            "kind": "Label",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 28,
                "col": 4
              },
              "end": {
                "line": 28,
                "col": 5
              }
            }
          },
          {
            "start": {
              "line": 27,
              "col": 4
            },
            "end": {
              "line": 28,
              "col": 3
            },
            "name": "y",
            "kind": "Label",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 27,
                "col": 4
              },
              "end": {
                "line": 27,
                "col": 5
              }
            }
          },
          {
            "start": {
              "line": 26,
              "col": 4
            },
            "end": {
              "line": 27,
              "col": 3
            },
            "name": "x",
            "kind": "Label",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 26,
                "col": 4
              },
              "end": {
                "line": 26,
                "col": 5
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 25,
            "col": 8
          },
          "end": {
            "line": 25,
            "col": 13
          }
        }
      },
      {
        "start": {
          "line": 23,
          "col": 0
        },
        "end": {
          "line": 23,
          "col": 50
        },
        "name": "eithery",
        "kind": "Type",
        "type": null,
        "children": [
          {
            "start": {
              "line": 23,
              "col": 36
            },
            "end": {
              "line": 23,
              "col": 50
            },
            "name": "Righty",
            "kind": "Constructor",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 23,
                "col": 38
              },
              "end": {
                "line": 23,
                "col": 44
              }
            }
          },
          {
            "start": {
              "line": 23,
              "col": 24
            },
            "end": {
              "line": 23,
              "col": 35
            },
            "name": "Lefty",
            "kind": "Constructor",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 23,
                "col": 24
              },
              "end": {
                "line": 23,
                "col": 29
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 23,
            "col": 14
          },
          "end": {
            "line": 23,
            "col": 21
          }
        }
      },
      {
        "start": {
          "line": 21,
          "col": 0
        },
        "end": {
          "line": 21,
          "col": 20
        },
        "name": "Ex",
        "kind": "Exn",
        "type": null,
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 21,
            "col": 10
          },
          "end": {
            "line": 21,
            "col": 12
          }
        }
      },
      {
        "start": {
          "line": 16,
          "col": 0
        },
        "end": {
          "line": 19,
          "col": 5
        },
        "name": "class_b",
        "kind": "Class",
        "type": null,
        "children": [
          {
            "start": {
              "line": 18,
              "col": 4
            },
            "end": {
              "line": 18,
              "col": 22
            },
            "name": "b",
            "kind": "Method",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 18,
                "col": 11
              },
              "end": {
                "line": 18,
                "col": 12
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 16,
            "col": 6
          },
          "end": {
            "line": 16,
            "col": 13
          }
        }
      },
      {
        "start": {
          "line": 12,
          "col": 0
        },
        "end": {
          "line": 14,
          "col": 3
        },
        "name": "class_type_a",
        "kind": "ClassType",
        "type": null,
        "children": [
          {
            "start": {
              "line": 13,
              "col": 2
            },
            "end": {
              "line": 13,
              "col": 23
            },
            "name": "a",
            "kind": "Method",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 13,
                "col": 2
              },
              "end": {
                "line": 13,
                "col": 23
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 12,
            "col": 11
          },
          "end": {
            "line": 12,
            "col": 23
          }
        }
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 10,
          "col": 3
        },
        "name": "Bar",
        "kind": "Module",
        "type": null,
        "children": [
          {
            "start": {
              "line": 9,
              "col": 2
            },
            "end": {
              "line": 9,
              "col": 27
            },
            "name": "b",
            "kind": "ClassType",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 9,
                "col": 13
              },
              "end": {
                "line": 9,
                "col": 14
              }
            }
          },
          {
            "start": {
              "line": 3,
              "col": 2
            },
            "end": {
              "line": 7,
              "col": 5
            },
            "name": "S1",
            "kind": "Signature",
            "type": null,
            "children": [
              {
                "start": {
                  "line": 6,
                  "col": 4
                },
                "end": {
                  "line": 6,
                  "col": 22
                },
                "name": "foo",
                "kind": "Value",
                "type": "t -> int",
                "children": [],
                "deprecated": false,
                "selection": {
                  "start": {
                    "line": 6,
                    "col": 8
                  },
                  "end": {
                    "line": 6,
                    "col": 11
                  }
                }
              },
              {
                "start": {
                  "line": 4,
                  "col": 4
                },
                "end": {
                  "line": 4,
                  "col": 10
                },
                "name": "t",
                "kind": "Type",
                "type": null,
                "children": [],
                "deprecated": false,
                "selection": {
                  "start": {
                    "line": 4,
                    "col": 9
                  },
                  "end": {
                    "line": 4,
                    "col": 10
                  }
                }
              }
            ],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 3,
                "col": 14
              },
              "end": {
                "line": 3,
                "col": 16
              }
            }
          },
          {
            "start": {
              "line": 2,
              "col": 2
            },
            "end": {
              "line": 2,
              "col": 14
            },
            "name": "t",
            "kind": "Type",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 2,
                "col": 7
              },
              "end": {
                "line": 2,
                "col": 8
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 1,
            "col": 7
          },
          "end": {
            "line": 1,
            "col": 10
          }
        }
      }
    ],
    "notifications": []
  }
  $ $MERLIN single outline < path.ml | jq '.value[].type'
  "A.a"
  null
  $ $MERLIN single outline -short-paths < path.ml | jq '.value[].type'
  "a"
  null
  $ $MERLIN single outline -filename foo.mli < foo.mli
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 56,
          "col": 0
        },
        "end": {
          "line": 56,
          "col": 29
        },
        "name": "final_let",
        "kind": "Value",
        "type": "< foo : int >",
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 56,
            "col": 4
          },
          "end": {
            "line": 56,
            "col": 13
          }
        }
      },
      {
        "start": {
          "line": 54,
          "col": 0
        },
        "end": {
          "line": 54,
          "col": 19
        },
        "name": "tb",
        "kind": "ClassType",
        "type": null,
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 54,
            "col": 4
          },
          "end": {
            "line": 54,
            "col": 6
          }
        }
      },
      {
        "start": {
          "line": 50,
          "col": 0
        },
        "end": {
          "line": 52,
          "col": 3
        },
        "name": "ta",
        "kind": "ClassType",
        "type": null,
        "children": [
          {
            "start": {
              "line": 51,
              "col": 2
            },
            "end": {
              "line": 51,
              "col": 35
            },
            "name": "baz",
            "kind": "Method",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 51,
                "col": 2
              },
              "end": {
                "line": 51,
                "col": 35
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 50,
            "col": 11
          },
          "end": {
            "line": 50,
            "col": 13
          }
        }
      },
      {
        "start": {
          "line": 32,
          "col": 0
        },
        "end": {
          "line": 32,
          "col": 19
        },
        "name": "tb",
        "kind": "ClassType",
        "type": null,
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 32,
            "col": 4
          },
          "end": {
            "line": 32,
            "col": 6
          }
        }
      },
      {
        "start": {
          "line": 30,
          "col": 0
        },
        "end": {
          "line": 30,
          "col": 26
        },
        "name": "ta",
        "kind": "ClassType",
        "type": null,
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 30,
            "col": 11
          },
          "end": {
            "line": 30,
            "col": 13
          }
        }
      },
      {
        "start": {
          "line": 22,
          "col": 0
        },
        "end": {
          "line": 22,
          "col": 42
        },
        "name": "point",
        "kind": "Type",
        "type": null,
        "children": [
          {
            "start": {
              "line": 22,
              "col": 34
            },
            "end": {
              "line": 22,
              "col": 40
            },
            "name": "z",
            "kind": "Label",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 22,
                "col": 34
              },
              "end": {
                "line": 22,
                "col": 35
              }
            }
          },
          {
            "start": {
              "line": 22,
              "col": 26
            },
            "end": {
              "line": 22,
              "col": 33
            },
            "name": "y",
            "kind": "Label",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 22,
                "col": 26
              },
              "end": {
                "line": 22,
                "col": 27
              }
            }
          },
          {
            "start": {
              "line": 22,
              "col": 18
            },
            "end": {
              "line": 22,
              "col": 25
            },
            "name": "x",
            "kind": "Label",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 22,
                "col": 18
              },
              "end": {
                "line": 22,
                "col": 19
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 22,
            "col": 8
          },
          "end": {
            "line": 22,
            "col": 13
          }
        }
      },
      {
        "start": {
          "line": 20,
          "col": 0
        },
        "end": {
          "line": 20,
          "col": 50
        },
        "name": "eithery",
        "kind": "Type",
        "type": null,
        "children": [
          {
            "start": {
              "line": 20,
              "col": 36
            },
            "end": {
              "line": 20,
              "col": 50
            },
            "name": "Righty",
            "kind": "Constructor",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 20,
                "col": 38
              },
              "end": {
                "line": 20,
                "col": 44
              }
            }
          },
          {
            "start": {
              "line": 20,
              "col": 24
            },
            "end": {
              "line": 20,
              "col": 35
            },
            "name": "Lefty",
            "kind": "Constructor",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 20,
                "col": 24
              },
              "end": {
                "line": 20,
                "col": 29
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 20,
            "col": 14
          },
          "end": {
            "line": 20,
            "col": 21
          }
        }
      },
      {
        "start": {
          "line": 18,
          "col": 0
        },
        "end": {
          "line": 18,
          "col": 20
        },
        "name": "Ex",
        "kind": "Exn",
        "type": null,
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 18,
            "col": 10
          },
          "end": {
            "line": 18,
            "col": 12
          }
        }
      },
      {
        "start": {
          "line": 10,
          "col": 0
        },
        "end": {
          "line": 12,
          "col": 3
        },
        "name": "class_type_a",
        "kind": "ClassType",
        "type": null,
        "children": [
          {
            "start": {
              "line": 11,
              "col": 2
            },
            "end": {
              "line": 11,
              "col": 23
            },
            "name": "a",
            "kind": "Method",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 11,
                "col": 2
              },
              "end": {
                "line": 11,
                "col": 23
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 10,
            "col": 11
          },
          "end": {
            "line": 10,
            "col": 23
          }
        }
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 8,
          "col": 3
        },
        "name": "Bar",
        "kind": "Module",
        "type": null,
        "children": [
          {
            "start": {
              "line": 4,
              "col": 2
            },
            "end": {
              "line": 7,
              "col": 5
            },
            "name": "S1",
            "kind": "Signature",
            "type": null,
            "children": [
              {
                "start": {
                  "line": 6,
                  "col": 4
                },
                "end": {
                  "line": 6,
                  "col": 22
                },
                "name": "foo",
                "kind": "Value",
                "type": "t -> int",
                "children": [],
                "deprecated": false,
                "selection": {
                  "start": {
                    "line": 6,
                    "col": 8
                  },
                  "end": {
                    "line": 6,
                    "col": 11
                  }
                }
              },
              {
                "start": {
                  "line": 5,
                  "col": 4
                },
                "end": {
                  "line": 5,
                  "col": 10
                },
                "name": "t",
                "kind": "Type",
                "type": null,
                "children": [],
                "deprecated": false,
                "selection": {
                  "start": {
                    "line": 5,
                    "col": 9
                  },
                  "end": {
                    "line": 5,
                    "col": 10
                  }
                }
              }
            ],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 4,
                "col": 14
              },
              "end": {
                "line": 4,
                "col": 16
              }
            }
          },
          {
            "start": {
              "line": 2,
              "col": 2
            },
            "end": {
              "line": 2,
              "col": 14
            },
            "name": "t",
            "kind": "Type",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 2,
                "col": 7
              },
              "end": {
                "line": 2,
                "col": 8
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 1,
            "col": 7
          },
          "end": {
            "line": 1,
            "col": 10
          }
        }
      }
    ],
    "notifications": []
  }

  $ $MERLIN single outline -filename path.mli < path.mli
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 0
        },
        "end": {
          "line": 5,
          "col": 11
        },
        "name": "x",
        "kind": "Value",
        "type": "A.a",
        "children": [],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 5,
            "col": 4
          },
          "end": {
            "line": 5,
            "col": 5
          }
        }
      },
      {
        "start": {
          "line": 1,
          "col": 0
        },
        "end": {
          "line": 3,
          "col": 3
        },
        "name": "A",
        "kind": "Module",
        "type": null,
        "children": [
          {
            "start": {
              "line": 2,
              "col": 2
            },
            "end": {
              "line": 2,
              "col": 14
            },
            "name": "a",
            "kind": "Type",
            "type": null,
            "children": [],
            "deprecated": false,
            "selection": {
              "start": {
                "line": 2,
                "col": 7
              },
              "end": {
                "line": 2,
                "col": 8
              }
            }
          }
        ],
        "deprecated": false,
        "selection": {
          "start": {
            "line": 1,
            "col": 7
          },
          "end": {
            "line": 1,
            "col": 8
          }
        }
      }
    ],
    "notifications": []
  }
  $ $MERLIN single outline < path.ml | jq '.value[].type'
  "A.a"
  null
  $ $MERLIN single outline -short-paths < path.ml | jq '.value[].type'
  "a"
  null

Check that when we pass "-include-types false", every "type" is null.
  $ $MERLIN single outline -include-types false < foo.ml \
  >   | jq '.value | .. | objects | select(has("type")) | .type'
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
  null
