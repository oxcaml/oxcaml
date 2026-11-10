import Howdah.Obligations
import Howdah.Determinism

howdah_import "even.json"

namespace Even

def even : Per := fun a b =>
  ∃ bits : BitVec 63,
    @a = @Val.immediate bits ∧ @b = @Val.immediate bits ∧ bits.srem 2 = 0

theorem even_isPer : IsPer even where
  symm := fun ⟨bits, ha, hb, h⟩ => ⟨bits, hb, ha, h⟩
  trans := fun ⟨bits, ha, hb, h⟩ ⟨bits', hb', hc, _⟩ => by
    cases Val.immediate_inj (hb.symm.trans hb')
    exact ⟨bits, ha, hc, h⟩

theorem even_refines : Per.Refines even Per.int :=
  fun ⟨bits, ha, hb, _⟩ => ⟨bits, ha, hb⟩

def verified : Verified where
  types := ⟨even⟩
  valid := ⟨even_isPer, even_refines⟩
  implementation := by
    apply Exists.intro
    refine ⟨?evaluates, ?wellTyped, ?laws⟩
    case evaluates =>
      unfold Even.implementation
      howdah_step Deterministic.letBindingBound Deterministic.function
      howdah_step Deterministic.letBindingBody
      howdah_step Deterministic.letBindingBound Deterministic.function
      howdah_step Deterministic.letBindingBody
      howdah_step Deterministic.primWithArgumentsReady (.cons (.cons .nil)) (.makeBlock rfl)
      exact .alreadyValue
    case wellTyped =>
      refine ⟨[_, _], [_, _], rfl, rfl, ?create, ?get, trivial⟩
      case create =>
        intro x y ⟨bits, hx, hy⟩
        subst hx hy
        by_cases parity : bits.srem 2 = 0
        · refine ⟨Val.some (Val.immediate bits), Val.some (Val.immediate bits),
            ?_, ?_, .inr ⟨_, _, rfl, rfl, bits, rfl, rfl, parity⟩⟩
          all_goals
            howdah_step Deterministic.applyClosure (.cons .nil) rfl
            howdah_step Deterministic.ifThenElseCondition (Deterministic.primArgs (Deterministic.head
              (Deterministic.primWithArgumentsReady (.cons (.cons .nil))
                (.intModNonzero (by decide)))
              (.cons .nil)))
            howdah_step Deterministic.ifThenElseCondition
              (Deterministic.primWithArgumentsReady (.cons (.cons .nil)) .physEqualImmediates)
            simp only [show bits.srem 2#63 = 0#63 from parity, ite_true]
            howdah_step Deterministic.ifThenElseTrue (by decide)
            howdah_step Deterministic.primWithArgumentsReady (.cons .nil) (.makeBlock rfl)
            exact .alreadyValue
        · refine ⟨Val.none, Val.none, ?_, ?_, .inl ⟨rfl, rfl⟩⟩
          all_goals
            howdah_step Deterministic.applyClosure (.cons .nil) rfl
            howdah_step Deterministic.ifThenElseCondition (Deterministic.primArgs (Deterministic.head
              (Deterministic.primWithArgumentsReady (.cons (.cons .nil))
                (.intModNonzero (by decide)))
              (.cons .nil)))
            howdah_step Deterministic.ifThenElseCondition
              (Deterministic.primWithArgumentsReady (.cons (.cons .nil)) .physEqualImmediates)
            simp only [show ¬bits.srem 2#63 = 0#63 from parity, ite_false]
            howdah_step Deterministic.ifThenElseFalse
            exact .alreadyValue
      case get =>
        intro x y related
        obtain ⟨bits, hx, hy, _⟩ := related
        subst hx hy
        refine ⟨Val.immediate bits, Val.immediate bits, ?_, ?_, bits, rfl, rfl⟩
        all_goals
          howdah_step Deterministic.applyClosure (.cons .nil) rfl
          exact .alreadyValue
    case laws =>
      refine ⟨fun x ⟨bits, hx, _, parity⟩ => ?_⟩
      subst hx
      howdah_step Deterministic.applyClosure (.cons (.cons .nil)) rfl
      howdah_step Deterministic.primArgs (Deterministic.head (Deterministic.primArgs (Deterministic.head
        (Deterministic.applyFunc (.cons .nil)
          (Deterministic.primWithArgumentsReady (.cons .nil) (.field (fields := [_, _]) rfl rfl)))
        (.cons .nil))) (.cons .nil))
      howdah_step Deterministic.primArgs (Deterministic.head (Deterministic.primArgs (Deterministic.head
        (Deterministic.applyClosure (.cons .nil) rfl)
        (.cons .nil))) (.cons .nil))
      howdah_step Deterministic.primArgs (Deterministic.head
        (Deterministic.primWithArgumentsReady (.cons (.cons .nil)) (.intModNonzero (by decide)))
        (.cons .nil))
      simp only [show bits.srem 2#63 = 0#63 from parity]
      howdah_step Deterministic.primWithArgumentsReady (.cons (.cons .nil)) .physEqualImmediates
      exact .alreadyValue

end Even

#howdah_check Even.verified
