<!--
     Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)

     SPDX-License-Identifier: CC-BY-SA-4.0
-->

<!-- title: capDL Language Specification -->

This document defines capDL revision 1.0, a language for capability
distributions.

# Background and Purpose

The purpose of capDL is describing snapshots of a system running on the
seL4 microkernel. In particular, it can be used to describe which
entities have access to which seL4 capabilities.

The main objectives are to use these descriptions for specifying
systems, for security analysis of systems, as input for automated
bootstrapping of systems, and for debugging applications running on
seL4. A capDL specification can be complete, i.e. give a full picture of
all capabilities in the system, or the system can be underspecified,
showing only the capabilities accessible to a particular component. It
is possible to describe system states in capDL that are not reachable
via actual system executions. It is also possible to leave out details
in a system specification that for instance are not important for a
security analysis (e.g. because the relevant authority is already
represented otherwise), but that would be necessary for an actual
implementation of the system.

# Syntax

This section gives a complete definition of the capDL syntax in BNF.

## Lexical Tokens

The lexical tokens of capDL are numbers (`number`), identifiers
(`identifier`), and verbatim symbols and operators typeset between
quotes `”` in the syntax section below.

Numbers are in decimal, hexadecimal (with prefix 0x), or octal (with
prefix 0) form. Identifiers start with a letter `[a-zA-Z]`, followed by
a sequence of letters `[a-zA-Z]`, numbers `[0-9]`, the underscore
character `_`, or a `@` character.

The language is case sensitive.

Comments can occur between any two tokens and can be nested. A
potentially multiline comment starts with the two characters `/*` and
ends with `*/`. A one-line comment starts with `–`. All characters after
`–` up to the end of the same line are ignored.

Whitespace (a sequence of space, newline and tab characters) can occur
between any two tokens and is ignored.

## Syntax

This section describes the concrete syntax of capDL using the tokens
defined above. The main syntactic entity of a file is `module` defined
in section [Modules](#modules).

### Names and Ranges

      name ::= identifier

      num ::= '[' number ']'
      range ::= number '..' number  | '..' number  | number  '..' | number
      ranges ::=  '[' ']' | '[' range (',' range)* ']'

      name_ref ::= name ranges?
      qname ::= name_ref ('/' name_ref)*

### Object Declarations

      obj_decls ::= 'objects' '{' obj_decl* '}'

      obj_decl ::= qname '=' object

      object ::= object_type object_params? opt_obj_decls?

      object_type ::= 'ep'  | 'notification' | 'tcb' | 'cnode' | 'ut' | 'irq' |
                      'asid_pool' | 'pt' | 'pd' | 'frame' | 'io_ports' |
                      'io_device' | 'io_pt' | 'vcpu'

      object_params ::= '(' (object_param (',' object_param)*)? ')'

      object_param   ::= obj_bit_size | obj_vm_size | io_pt_level | obj_ports_size |
                         init_arguments | tcb_dom | obj_paddr | domain_id |
                         pci_device
      obj_bit_size   ::= number 'bits'
      obj_vm_size    ::= number ('k' | 'M')
      io_pt_level    ::= 'level' ':' number
      obj_ports_size ::= number 'k' 'ports'
      init_arguments ::= 'init' ':' '[' (number (',' number)*)? ']'
      tcb_dom        ::= 'dom' ':' number
      obj_paddr      ::= 'paddr' ':' number
      domain_id      ::= 'domainID' ':' number
      pci_device     ::= number ':' number '.' number

      opt_obj_decls ::= '{' ((obj_decl | name_ref) ','?)* '}'

### Capability Declarations

      cap_decls ::= 'caps' '{' (cap_name_decl | cap_decl)* '}'

      cap_name_decl ::= name '=' '(' name_ref ',' slot ')'

      slot ::= number | symbolic_slot
      symbolic_slot ::= 'cspace' | 'vspace' | 'reply_slot' | 'caller_slot' |
                        'ipc_buffer_slot'

      cap_decl ::= name_ref '{' (cap_mapping_or_ref ';'?)* '}'

      cap_mapping_or_ref ::= (slot ':')? cap_name? (cap_mapping | cap_name_ref)

      cap_name    ::= name_ref '='
      cap_mapping ::= name_ref cap_params? parent?
      cap_params  ::= '(' cap_param (',' cap_param)* ')'

      cap_param ::= right+
                  | 'masked' ':' right+
                  | 'guard' ':' number
                  | 'guard_size' ':' number
                  | 'badge' ':' number
                  | 'ports'  ':' ranges
                  | 'reply'
                  | 'master_reply'
                  | 'asid' ':' asid
                  | 'cached'
                  | 'uncached'

      right ::= 'R' | 'W' | 'G' | 'X'
      asid ::= '(' number ',' number ')'

      parent ::= '- child_of' slot_ref

      slot_ref ::= '(' name_ref ',' slot ')' | name_ref

      cap_name_ref ::= '<' name_ref '>' cap_params? parent?

### IRQ Declarations

      irq_decls ::='irq_maps' '{' (irq_mapping ';'?)* '}'

      irq_mapping ::= (number ':')? name_ref

### CDT Declarations

      cdt_decls ::='cdt' '{' cdt_decl* '}'

      cdt_decl ::= slot_ref '{' ((slot_ref | cdt_decl) ';'?)* '}'

### Modules

      arch ::= 'arch' ('ia32' | 'arm11')

      module ::= arch (obj_decls | cap_decls | irq_decls | cdt_decls)+

# Semantics

In this section we show the semi-formal semantics of capDL in the form
of its underlying data model in Haskell and give a brief rationale for
its contents. This data model can be made fully formal by mapping it
directly into the Isabelle theorem prover via Data61's existing
Haskell-to-Isabelle translation tool. The data model can also be mapped
into various output formats.

After describing the data model below, we outline a semi-formal
definition of the semantics: mapping the capDL syntax into the capDL
data model.

## Data Model

### Rationale
The purpose of capDL is describing a snapshot of the capability
distribution in a system running on the seL4 microkernel. For this
purpose, we need to know which objects exist in the system and which
capabilities they have access to. The second purpose of capDL is to
serve as sufficiently detailed input to automated code generation
tools. Therefore we need to include all information about capability
arguments that such implementations will need. Some of this information
will not be relevant for security analysis. For instance, for a
security analysis it may be necessary to know which frames of physical
memory an entity in the system can access via virtual memory, but it is
not necessary to know under which virtual address each of these
physical addresses is visible to the process. For a concrete
implementation of a bootstrapping component on the other hand, this
latter information is crucial.

### The Model

We refer to objects by name. This name could be of any type; for
convenience we either use a plain string or a string with an index. We
use the `Maybe` type of Haskell to express this.

    types ObjID = (String, Maybe Word)

With this the capability state of a system is fully described by a map
from `ObjID` to `Object`. Since not all objects and capabilities are
supported by seL4 on all machine architectures, we also store which
architecture the system is intended for.

    data Model =  Model {
                      arch :: Arch,
                      objects :: Map ObjID Object
                  }

    data Arch = IA32 | ARM11

Objects are described by the following data type. We are mainly
interested in what capabilities an object contains. We also store
information that is relevant for creating an object such as its type and
its size when the size is configurable. This means in contrast to the
security model, where we only talk about abstract entities, we give more
detailed information in capDL.

    types CapMap = Map Word Cap

    data Object = Endpoint
                | Notification
                | TCB { slots :: CapMap, initArguments :: [Word] }
                | CNode { slots :: CapMap, sizeBits :: Word }
                | Untyped { maybeSizeBits :: Maybe Word }

                | ASIDPool { slots :: CapMap }
                | PT { slots :: CapMap }
                | PD { slots :: CapMap }
                | Frame { vmSizeBits :: Word }
                | IOPorts { size :: Word }
                | IOPT { slots :: CapMap, level :: Word }
                | IODevice { slots :: CapMap }

The first two objects, communication endpoints and notifications, do not
store any capabilities and have a fixed size. Thread Control Blocks
(TCB) contain a small number of capability registers, modelled by the
field `slots`, a map from a machine word (the slot/register number) to
the capability that is stored in that slot. Slots may be empty. CNode
objects are the main capability storage object in seL4. They have
configurable size. Untyped objects are conceptual containers for
dynamically created objects. They cover a set of objects referred to by
their `ObjID`. This set is called the covering set. In the
implementation untyped objects must have a size, but in capDL
specifications we often want to leave the size unspecified (as large as
necessary).

The next batch of objects in the data type above are architecture
dependent. Most of these data structures and devices do not store actual
capabilities in the kernel and hardware implementation. Instead they
store pointers or mappings to other resources in the system. Since these
resources are already represented as objects, we use capabilities to
model these mappings and to express the access, for instance, a device
may have to physical memory. In detail, the architecture dependent
objects are as follows. ASID pools store which page directories (PD) can
be activated by the machine. As mentioned, we represent this mapping as
a capability in the model. Analogously, we model the mappings in virtual
memory objects page directory (PD) and page table (PT). Physical memory
frames (`Frame`) come in different sizes, depending on architecture, but
do not contain further capabilities. IOMMU page tables (`IOPT`) come in
multiple levels and again store mappings to frames or further page
tables. Finally, hardware devices are represented by the object type
`IODevice`. Again, we model the access hardware may have to resources in
the system as capabilities. Specifically, IODevices may have a
capability to a CNode for IRQ delivery, potentially multiple
capabilities to physical memory frames for memory mapped device
registers, and one capability for the root IOMMU space.

To conclude the description of the capDL data model, it remains to
define the data type of capabilities `Cap`. Almost all capabilities in
seL4 refer to one specific object. Some may refer to a set of objects or
implicitly to all objects of a given type. Many of the capabilities
store explicit access rights. These are modelled as follows:

    data Rights = Read | Write | Grant | GrantReply
    types CapRights = Set Right

Again in contrast to the security model of seL4, we explicitly
distinguish different types of capabilities in capDL and store slightly
different kinds of additional information with each. As a side effect,
we do not need an explicit representation of the create right. The
create right is conferred by the possession of an untyped capability.

    data Cap = NullCap
             | UntypedCap { capObj :: ObjID }
             | EndpointCap { capObj :: ObjID, capBadge :: Word,
                             capRights :: CapRights }
             | NotificationCap { capObj :: ObjID, capBadge :: Word,
                                 capRights :: CapRights }
             | ReplyCap { capObj :: ObjID }
             | MasterReplyCap { capObj :: ObjID }
             | CNodeCap { capObj :: ObjID, capGuard :: Word,
                          capGuardSize :: Word }
             | TCBCap { capObj :: ObjID }
             | IRQControlCap
             | IRQHandlerCap { capObj :: ObjID }
             | FrameCap { capObj :: ObjID, capRights :: CapRights }
             | PTCap { capObj :: ObjID }
             | PDCap { capObj :: ObjID }
             | ASIDControlCap
             | ASIDPoolCap { capObj :: ObjID }
             | IOPortsCap { capObjs :: Set Word }
             | IOSpaceMasterCap
             | IOSpaceCap { capObj :: ObjID }
             | IOPTCap { capObj :: ObjID }

In detail, the capabilities are as follows. We go through the list of
capabilities and give a brief indication of the authority they convey.
The `NullCap` is occasionally used to represent the absence of a
capability. An untyped capability points to an untyped object and
confers the right to issue create/retype and revoke/delete operations.
Endpoint and notification capabilities point to their respective object
types. They explicitly store which Read/Write/Grant access is conferred
to the communications channel and they may carry one machine word of
additional information, called the badge. CNode capabilities in addition
to their CNode object reference carry information that influences the
lookup of capabilities in the node they point to. This information
consists of a guard in the sense of guarded page tables, the size of
that guard, and finally the rights to the CNode object itself. A TCB cap
gives authority over one TCB object with given access rights. The
IRQControl capability gives authority to create specific IRQHandler
capabilities. Each device may have a special CNode associated with it
that may contain a notification which in turn is used to deliver
interrupts to user level. IRQHandler caps in capDL point to these
specific CNodes. It confers the authority to change which endpoint the
interrupt is delivered to. Frame capabilities confer authority to map
and unmap physical memory frames into and from a page directory or page
table with the specified access rights. PT caps give authority to
map/unmap page tables into/from page directories and PD caps give the
authority to map/unmap page directories into ASID pools and to install
them as virtual memory space of a process. The ASIDControlCap together
with an Untyped cap confers authority to create new ASID pools. Only a
limited, fixed number can be created in the system. ASIDPool caps give
the authority to map/unmap page directories into/from the ASID pool.
Mapping a frame into a page table for instance, needs the frame cap to
specify which frame to map with which rights and the PT cap to specify
in which page table. The IOPorts capability gives access to a set of
IOPorts on the ia32 architecture. The IOSpaceMasterCap confers the
authority to create IOSpace caps for specific devices. IOSpaceCaps point
to IODevices and confer the authority to set the root IOPageTable for
that device. IOPTCaps are the IOMMU analogue to PT and PD caps in normal
virtual memory.

## Semantics

In this section, we define a mapping from the syntax of capDL to the
data model of capDL. The mapping is defined by going over the relevant
productions in the capDL grammar and describing the corresponding model.

### Names and Ranges

      name ::= identifier

Names are mapped to `ObjID`s in the model. A name without index maps to
`(name, Nothing)`. Qualified names will be defined in the object
declaration section.

      num ::= '[' number ']'

This production maps to a single index and is used to either declare a
set of objects, in which case the number is the number of objects, or to
refer to a specific index. The term name\[x\] maps to the `ObjID`
`(name, Just x)`.

      range ::= number '..' number  | '..' number  | number  '..' | number
      ranges ::=  '[' ']' | '[' range (',' range)* ']'
      name_ref ::= name ranges?

Ranges stand for sets of `ObjID`s. For a set declared as `name[n]`, the
following mappings apply:

-   `name[..b]` refers to `name[0]`, `name[1]`, .., `name[b]`

-   `name[a..]` refers to `name[a]`, `name[a+1]`, .., `name[n]`

-   `name[a..b]` refers to `name[a]`, `name[a+1]`, .., `name[b]`

-   `name[]` refers to `name[0]`, `name[1]`, .., `name[n]`

A list of ranges `name[r1,r2,..r3]` refers to the union of the ranges
`name[r1]`, `name[r2]`, .., `name[r3]`.

### Module

      arch ::= 'arch' ('ia32' | 'arm11')
      module ::= arch (obj_decls | cap_decls)+

A module maps to a full `Model` in the data model. Its `Arch` component
is determined by `arch`, its mapping from `ObjID` to `Object` by the
object and capability declarations described below.

### Object Declarations

      obj_decls ::= 'objects' obj_decl*

An object declaration section defines a set of objects. Each object name
of typed objects should be declared only once. The covering set of
untyped objects may be declared multiple times. The total covering set
is the union of all covering sets mentioned for that object.

      obj_decl ::= qname '=' object

An object declaration has a potentially qualified name and dimension on
the left hand side and an object content declaration on the right hand
side. A qualified name `name1/name2/name3[n]` implicitly declares the
untyped objects `name1` and `name2`. It also declares that `name2` is
covered by `name1` and `name3` covered by `name2`. Giving a dimension
`[n]` means that n objects `(name3, Just 0)` to `(name3, Just (n-1))`
are declared, each with the same content described on the right hand
side of the equation.

The right hand side is defined by the following syntax productions:

      object ::= object_type object_params? obj_decls?

      object_type ::= 'ep'  | 'notification' | 'tcb' | 'cnode' | 'ut' |
                      'asid_pool' | 'pt' | 'pd' | 'frame' | 'io_ports' |
                      'io_device' | 'io_pt'

      object_params ::= '(' object_param (',' object_param)* ')'

      object_param   ::= obj_bit_size | obj_vm_size | io_pt_level | obj_ports_size
      obj_bit_size   ::= number 'bits'
      obj_vm_size    ::= number ('k' | 'M')
      io_pt_level    ::= 'level' ':' ('1' | '2' | '3')
      obj_ports_size ::= number 'k' 'ports'

      obj_decls ::= '{' ((obj_decl | name_ref) ','?)* '}'

The object content is given by the type of the object mapping to its
corresponding object type in the data model. Some of the object types in
the model require a size parameter, given in bits (where n bits means a
size of 2^n entries). Frames' sizes are specified directly as are the
levels of IOMMU page tables. Untyped objects can be followed by a nested
object declaration. Writing

    name1 = ut {
      name2/name3 [n] = object
      name2/name4 = object
     ...
    }

is equivalent to writing

    name1/name2/name3 [n] = object
    name1/name2/name4 = object
    ...

### Capability Declarations

The capability content of objects is declared in its own section,
separately from the list of objects itself.

      cap_decls = 'caps' (cap_name_decl | cap_decl)*

      cap_name_decl ::= name '=' '(' name_ref ',' slot ')'
      slot ::= number | symbolic_slot
      symbolic_slot ::= 'cspace' | 'vspace'

Next to capability content declarations, there are name declarations for
capability slots. A capability slot is defined by the container object,
referred to by a name reference (mapping to a single `ObjID`) and a slot
inside the container specified by its slot number. The symbolic slot
names `cspace`, `vspace` stand for slot numbers 0 and 1.

A capability declaration starts with a set of container objects, each of
which is declared to contain the capability mappings specified on the
right hand side. Container objects may occur more than once in a
specification. Their mappings are the union of all mappings for that
container object.

      cap_decl ::= name_ref '{' (cap_mapping_or_ref ';'?)* '}'

A cap mapping is specified by its slot and its capability and
potentially an inline name declaration for the specified slot in the
specified container.

      cap_mapping_or_ref ::= slot ':' cap_name? (cap_mapping | cap_name_ref)

      cap_name    ::= name '='
      cap_mapping ::= name_ref cap_params?
      cap_params  ::= '(' cap_param (',' cap_param)* ')'

      cap_param ::= right+
                  | 'masked' ':' right+
                  | 'guard' ':' number
                  | 'guard_size' ':' number
                  | 'irq'
                  | 'badge' ':' number
                  | 'reply'
                  | 'master_reply'

      right ::= 'R' | 'W' | 'G'

      cap_name_ref ::= '<' name_ref '>' cap_params?

The capability declaration is either a `cap_mapping` or `cap_name_ref`.
In the latter case the mapping is a copy of the capability in the slot
referred to by `name` (possibly `masked` with an access rights mask). In
the former case, a cap is defined by the object it points to, together
with optional parameters depending on the type of the object in the data
model. For instance, an endpoint cap is specified if the `ObjID` given
in the cap declaration refers to an endpoint object. If no parameters
are mentioned, default parameters are substituted if possible. These
defaults are: empty set of rights for access rights, full set of rights
for rights masks, guard 0, guard size 0, and badge 0. The IRQControl cap
is specified using the reserved `ObjID` `irq_control`, similarly
ASIDControlCap is specified by `asid_control` and IOSpaceMasterCap by
`io_space_master`.
