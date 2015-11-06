package io.bimble.pqgram

import org.scalacheck.{Gen, Prop}
import org.scalatest.{Matchers, FlatSpec}

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.constrained.DAG


class PQGramRegressions extends FlatSpec with Matchers {

  import Implicits._

  case class Label(id: String, i: Int)

  implicit object NodeLabeller extends Labelled[Label] {
    override def label(a: Label): String = a.i.toString
  }

  implicit val ord = new CreatableOrdering[Label] {
    override def compare(x: Label, y: Label): Int = x.i compare y.i
    override def createLessThan(x: Label): Label = Label(Gen.uuid.sample.map(_.toString).get, x.i - 1)
    override def createGreaterThan(x: Label): Label = Label(Gen.uuid.sample.map(_.toString).get, x.i + 1)
    override def create: Label = Label(Gen.uuid.sample.map(_.toString).get, 0)
  }


  val arg0 = DAG.from(List(
    Label("c2b579bf-49ca-4776-be78-81fb6445e567",97), Label("1c6287d9-1a0a-4909-823d-e4d0193ac836",11),
    Label("e51d9b63-4681-4117-ad95-dc0ac644459b",51), Label("84a1b978-9037-4f36-becd-704a358c7001",97),
    Label("7de768e8-424c-4382-8589-4f0d6dfc4af5",41), Label("3f0e43cd-85d2-4e1e-8ba8-a39a09449cc4",23),
    Label("2b3a654c-6526-4822-9ca2-11020963a24d",26), Label("6ac55466-e495-491a-bb95-d9eb7b30c750",54),
    Label("2db00d2b-9371-4a8d-9493-6053f5b57293",1), Label("ae4847c1-b62e-4dc7-b955-28b739bf2394",74),
    Label("c5370d9d-f920-4666-befe-cfabecfe649c",6), Label("079e26f5-88d6-47de-a055-8a313210b538",12),
    Label("57674087-c878-40a1-942f-14986bfe7414",68), Label("8222a831-3007-443d-aa28-32b06856019a",13),
    Label("67536403-6e55-4681-ba4c-605953485098",17), Label("1d2fd03a-cd9c-4b61-851e-555c98bede6f",42),
    Label("c3989e5d-1d2a-4d5d-98cf-8bd9aa761bc9",58), Label("69bce20f-a52b-4f82-8902-f654b3a82197",17),
    Label("658d575d-6a00-48cc-9fee-b4f918dac4b6",81), Label("20b56d6b-3240-47d8-a16b-600497fbbc34",17),
    Label("1f7ea6a7-ebc9-4621-958d-f3b692380cc5",24), Label("81f1b75f-bcb8-4d04-9a2b-fe1a350e873d",45),
    Label("58dd98e9-8f96-4254-b541-e26452b596a0",5), Label("f8d44acd-e082-4856-a528-5d6d7183a649",60),
    Label("6a166829-06eb-4ad4-bd92-68a19b78f7fe",100), Label("0f470000-0f82-42fb-9133-8ac09360c192",8),
    Label("5e8534f1-8627-4ea1-a328-f9496bae8b0f",50), Label("dc35a6b0-860e-4abf-8a04-ade235596f9d",4),
    Label("a85c697a-8fbe-4c8e-a10b-ec399d45d0b6",86), Label("58a42ef6-1bc9-40b8-a816-2f64ef327484",53),
    Label("72991b1a-ce88-4fe4-93ff-c2c4c31bd862",1), Label("f021dff9-80a6-4ca9-96ce-f8623cba1783",8),
    Label("fbfa664f-9787-43da-ac03-49c7117beeb1",34), Label("bc4a4365-ae2a-4810-a693-869ca7e1bb8b",17),
    Label("4d0b0a42-8b7c-4e78-b2a5-904ab3658254",39), Label("7736e00c-5b01-4890-a05a-54f416a9478a",18),
    Label("e634fbba-df24-4e47-b4bd-20576c883ab4",64), Label("c8220083-0b57-4844-b327-f167bebc3650",83),
    Label("cb517457-ea85-43c7-9916-1dd2fe701e50",97), Label("3c60ad23-22d4-46ff-8718-b30280e5487a",76),
    Label("c78c3d81-935c-4a51-babd-8b758a212b51",50), Label("cd5cdb5b-d6d0-4714-9b17-90427cfff6f2",64),
    Label("e119d736-2232-4ad1-beae-e83383f4389f",49), Label("fcebf3c1-b556-4477-97a5-ee908b43e529",33),
    Label("a991cfbf-fa60-46e4-bd03-31ab2d85eace",5), Label("3bd306eb-74e0-4d9e-b615-836d80ac2890",97),
    Label("44ccb009-1141-40cb-94c8-98a41db6d589",85), Label("bbaa015e-1699-4c9e-87b8-ce8aac083106",9),
    Label("587f02ee-484d-4b4c-b645-c332514befb2",51), Label("7f0b6b5c-c934-4a36-8663-2fb978e3e444",43),
    Label("95c1f82e-2ddf-4404-8159-2809c09f650d",77), Label("bcfacb38-fa75-40c6-a711-07715cfdbd98",39),
    Label("4fba110d-80c4-4189-8920-47d15a3b40d0",23), Label("9b704ea7-6b44-45d6-80ba-722c9bb1368c",100),
    Label("0ae16005-ac1e-4bd0-acc7-0c81dcd3169f",71), Label("bf9d343f-774c-4b62-b3e8-9af6139ccd5c",44),
    Label("3fdd527a-bfc6-4202-95c3-4c1da15f1a1b",17), Label("6de2c73a-47cd-40c5-9149-9f94a5823fad",66),
    Label("fa64640b-7ad8-43c7-8f69-d7318dd5d254",19), Label("1165406c-c2ce-45ef-977d-3f58405ef515",92),
    Label("94f67ddc-9ec8-45c9-9637-b7249ca7465a",11), Label("1a58dbeb-b831-4474-aa71-cc1a37f467ac",28),
    Label("1d82b01d-934f-402e-9465-e54593917a3a",47), Label("ab8092d3-dfb9-4206-b16d-0b511adf4f65",14),
    Label("2486bf8e-0fdc-45cb-9b98-cedc94455261",95), Label("4bf84773-fa07-4de3-82c3-c6ad88f34e29",34),
    Label("da07864a-a491-45c9-b63c-db34e4b1fa27",12), Label("5b0ed5f5-6ad5-4e67-bbc9-9aecd8e3256b",82),
    Label("38dffaa9-4e7f-4c62-b488-801d01ecf9ab",77), Label("13a38ee7-7235-42b0-b652-01b61d31cd4a",33),
    Label("2cb781db-2f05-4a1b-b64d-ca0734c4c23c",36), Label("95f2cc8b-3164-4f6a-af40-c2524a4603fc",13)
  ), List(
    Label("c2b579bf-49ca-4776-be78-81fb6445e567",97)~>Label("5b0ed5f5-6ad5-4e67-bbc9-9aecd8e3256b",82),
    Label("c2b579bf-49ca-4776-be78-81fb6445e567",97)~>Label("c3989e5d-1d2a-4d5d-98cf-8bd9aa761bc9",58),
    Label("c2b579bf-49ca-4776-be78-81fb6445e567",97)~>Label("ab8092d3-dfb9-4206-b16d-0b511adf4f65",14),
    Label("1c6287d9-1a0a-4909-823d-e4d0193ac836",11)~>Label("84a1b978-9037-4f36-becd-704a358c7001",97),
    Label("1c6287d9-1a0a-4909-823d-e4d0193ac836",11)~>Label("bcfacb38-fa75-40c6-a711-07715cfdbd98",39),
    Label("1c6287d9-1a0a-4909-823d-e4d0193ac836",11)~>Label("1d2fd03a-cd9c-4b61-851e-555c98bede6f",42),
    Label("1c6287d9-1a0a-4909-823d-e4d0193ac836",11)~>Label("e119d736-2232-4ad1-beae-e83383f4389f",49),
    Label("84a1b978-9037-4f36-becd-704a358c7001",97)~>Label("cb517457-ea85-43c7-9916-1dd2fe701e50",97),
    Label("84a1b978-9037-4f36-becd-704a358c7001",97)~>Label("a991cfbf-fa60-46e4-bd03-31ab2d85eace",5),
    Label("84a1b978-9037-4f36-becd-704a358c7001",97)~>Label("2db00d2b-9371-4a8d-9493-6053f5b57293",1),
    Label("84a1b978-9037-4f36-becd-704a358c7001",97)~>Label("95f2cc8b-3164-4f6a-af40-c2524a4603fc",13),
    Label("2b3a654c-6526-4822-9ca2-11020963a24d",26)~>Label("1f7ea6a7-ebc9-4621-958d-f3b692380cc5",24),
    Label("2b3a654c-6526-4822-9ca2-11020963a24d",26)~>Label("587f02ee-484d-4b4c-b645-c332514befb2",51),
    Label("6ac55466-e495-491a-bb95-d9eb7b30c750",54)~>Label("69bce20f-a52b-4f82-8902-f654b3a82197",17),
    Label("6ac55466-e495-491a-bb95-d9eb7b30c750",54)~>Label("da07864a-a491-45c9-b63c-db34e4b1fa27",12),
    Label("6ac55466-e495-491a-bb95-d9eb7b30c750",54)~>Label("67536403-6e55-4681-ba4c-605953485098",17),
    Label("079e26f5-88d6-47de-a055-8a313210b538",12)~>Label("7de768e8-424c-4382-8589-4f0d6dfc4af5",41),
    Label("079e26f5-88d6-47de-a055-8a313210b538",12)~>Label("94f67ddc-9ec8-45c9-9637-b7249ca7465a",11),
    Label("57674087-c878-40a1-942f-14986bfe7414",68)~>Label("c2b579bf-49ca-4776-be78-81fb6445e567",97),
    Label("57674087-c878-40a1-942f-14986bfe7414",68)~>Label("0ae16005-ac1e-4bd0-acc7-0c81dcd3169f",71),
    Label("57674087-c878-40a1-942f-14986bfe7414",68)~>Label("2486bf8e-0fdc-45cb-9b98-cedc94455261",95),
    Label("1d2fd03a-cd9c-4b61-851e-555c98bede6f",42)~>Label("fcebf3c1-b556-4477-97a5-ee908b43e529",33),
    Label("1d2fd03a-cd9c-4b61-851e-555c98bede6f",42)~>Label("fa64640b-7ad8-43c7-8f69-d7318dd5d254",19),
    Label("1d2fd03a-cd9c-4b61-851e-555c98bede6f",42)~>Label("e51d9b63-4681-4117-ad95-dc0ac644459b",51),
    Label("20b56d6b-3240-47d8-a16b-600497fbbc34",17)~>Label("58a42ef6-1bc9-40b8-a816-2f64ef327484",53),
    Label("20b56d6b-3240-47d8-a16b-600497fbbc34",17)~>Label("bf9d343f-774c-4b62-b3e8-9af6139ccd5c",44),
    Label("20b56d6b-3240-47d8-a16b-600497fbbc34",17)~>Label("38dffaa9-4e7f-4c62-b488-801d01ecf9ab",77),
    Label("58dd98e9-8f96-4254-b541-e26452b596a0",5)~>Label("c8220083-0b57-4844-b327-f167bebc3650",83),
    Label("58dd98e9-8f96-4254-b541-e26452b596a0",5)~>Label("6de2c73a-47cd-40c5-9149-9f94a5823fad",66),
    Label("6a166829-06eb-4ad4-bd92-68a19b78f7fe",100)~>Label("81f1b75f-bcb8-4d04-9a2b-fe1a350e873d",45),
    Label("6a166829-06eb-4ad4-bd92-68a19b78f7fe",100)~>Label("13a38ee7-7235-42b0-b652-01b61d31cd4a",33),
    Label("6a166829-06eb-4ad4-bd92-68a19b78f7fe",100)~>Label("2cb781db-2f05-4a1b-b64d-ca0734c4c23c",36),
    Label("6a166829-06eb-4ad4-bd92-68a19b78f7fe",100)~>Label("e634fbba-df24-4e47-b4bd-20576c883ab4",64),
    Label("72991b1a-ce88-4fe4-93ff-c2c4c31bd862",1)~>Label("2b3a654c-6526-4822-9ca2-11020963a24d",26),
    Label("72991b1a-ce88-4fe4-93ff-c2c4c31bd862",1)~>Label("bc4a4365-ae2a-4810-a693-869ca7e1bb8b",17),
    Label("72991b1a-ce88-4fe4-93ff-c2c4c31bd862",1)~>Label("6a166829-06eb-4ad4-bd92-68a19b78f7fe",100),
    Label("72991b1a-ce88-4fe4-93ff-c2c4c31bd862",1)~>Label("6ac55466-e495-491a-bb95-d9eb7b30c750",54),
    Label("bc4a4365-ae2a-4810-a693-869ca7e1bb8b",17)~>Label("bbaa015e-1699-4c9e-87b8-ce8aac083106",9),
    Label("bc4a4365-ae2a-4810-a693-869ca7e1bb8b",17)~>Label("9b704ea7-6b44-45d6-80ba-722c9bb1368c",100),
    Label("bc4a4365-ae2a-4810-a693-869ca7e1bb8b",17)~>Label("dc35a6b0-860e-4abf-8a04-ade235596f9d",4),
    Label("bc4a4365-ae2a-4810-a693-869ca7e1bb8b",17)~>Label("1165406c-c2ce-45ef-977d-3f58405ef515",92),
    Label("c8220083-0b57-4844-b327-f167bebc3650",83)~>Label("1d82b01d-934f-402e-9465-e54593917a3a",47),
    Label("c8220083-0b57-4844-b327-f167bebc3650",83)~>Label("4d0b0a42-8b7c-4e78-b2a5-904ab3658254",39),
    Label("c8220083-0b57-4844-b327-f167bebc3650",83)~>Label("1c6287d9-1a0a-4909-823d-e4d0193ac836",11),
    Label("c78c3d81-935c-4a51-babd-8b758a212b51",50)~>Label("8222a831-3007-443d-aa28-32b06856019a",13),
    Label("c78c3d81-935c-4a51-babd-8b758a212b51",50)~>Label("7736e00c-5b01-4890-a05a-54f416a9478a",18),
    Label("c78c3d81-935c-4a51-babd-8b758a212b51",50)~>Label("a85c697a-8fbe-4c8e-a10b-ec399d45d0b6",86),
    Label("c78c3d81-935c-4a51-babd-8b758a212b51",50)~>Label("f8d44acd-e082-4856-a528-5d6d7183a649",60),
    Label("e119d736-2232-4ad1-beae-e83383f4389f",49)~>Label("c5370d9d-f920-4666-befe-cfabecfe649c",6),
    Label("e119d736-2232-4ad1-beae-e83383f4389f",49)~>Label("658d575d-6a00-48cc-9fee-b4f918dac4b6",81),
    Label("7f0b6b5c-c934-4a36-8663-2fb978e3e444",43)~>Label("1a58dbeb-b831-4474-aa71-cc1a37f467ac",28),
    Label("7f0b6b5c-c934-4a36-8663-2fb978e3e444",43)~>Label("4fba110d-80c4-4189-8920-47d15a3b40d0",23),
    Label("7f0b6b5c-c934-4a36-8663-2fb978e3e444",43)~>Label("20b56d6b-3240-47d8-a16b-600497fbbc34",17),
    Label("7f0b6b5c-c934-4a36-8663-2fb978e3e444",43)~>Label("4bf84773-fa07-4de3-82c3-c6ad88f34e29",34),
    Label("4fba110d-80c4-4189-8920-47d15a3b40d0",23)~>Label("fbfa664f-9787-43da-ac03-49c7117beeb1",34),
    Label("4fba110d-80c4-4189-8920-47d15a3b40d0",23)~>Label("5e8534f1-8627-4ea1-a328-f9496bae8b0f",50),
    Label("0ae16005-ac1e-4bd0-acc7-0c81dcd3169f",71)~>Label("3f0e43cd-85d2-4e1e-8ba8-a39a09449cc4",23),
    Label("0ae16005-ac1e-4bd0-acc7-0c81dcd3169f",71)~>Label("0f470000-0f82-42fb-9133-8ac09360c192",8),
    Label("6de2c73a-47cd-40c5-9149-9f94a5823fad",66)~>Label("72991b1a-ce88-4fe4-93ff-c2c4c31bd862",1),
    Label("6de2c73a-47cd-40c5-9149-9f94a5823fad",66)~>Label("57674087-c878-40a1-942f-14986bfe7414",68),
    Label("6de2c73a-47cd-40c5-9149-9f94a5823fad",66)~>Label("7f0b6b5c-c934-4a36-8663-2fb978e3e444",43),
    Label("6de2c73a-47cd-40c5-9149-9f94a5823fad",66)~>Label("079e26f5-88d6-47de-a055-8a313210b538",12),
    Label("1a58dbeb-b831-4474-aa71-cc1a37f467ac",28)~>Label("95c1f82e-2ddf-4404-8159-2809c09f650d",77),
    Label("1a58dbeb-b831-4474-aa71-cc1a37f467ac",28)~>Label("cd5cdb5b-d6d0-4714-9b17-90427cfff6f2",64),
    Label("1a58dbeb-b831-4474-aa71-cc1a37f467ac",28)~>Label("3c60ad23-22d4-46ff-8718-b30280e5487a",76),
    Label("1d82b01d-934f-402e-9465-e54593917a3a",47)~>Label("c78c3d81-935c-4a51-babd-8b758a212b51",50),
    Label("1d82b01d-934f-402e-9465-e54593917a3a",47)~>Label("ae4847c1-b62e-4dc7-b955-28b739bf2394",74),
    Label("4bf84773-fa07-4de3-82c3-c6ad88f34e29",34)~>Label("f021dff9-80a6-4ca9-96ce-f8623cba1783",8),
    Label("4bf84773-fa07-4de3-82c3-c6ad88f34e29",34)~>Label("3bd306eb-74e0-4d9e-b615-836d80ac2890",97),
    Label("4bf84773-fa07-4de3-82c3-c6ad88f34e29",34)~>Label("44ccb009-1141-40cb-94c8-98a41db6d589",85),
    Label("4bf84773-fa07-4de3-82c3-c6ad88f34e29",34)~>Label("3fdd527a-bfc6-4202-95c3-4c1da15f1a1b",17)
  ))

  val arg1 = DAG.from(List(
    Label("78a77011-d93b-4f31-9a0e-f8f247b62f64",26), Label("61e8630d-74e0-4585-aecb-09f4bd19c851",95),
    Label("06cd64af-82b6-423f-9709-3b639f04a374",17), Label("91e48588-05c7-4b2d-b880-6daed1e016fa",38),
    Label("21365d6c-d0a3-4ecc-a5ed-5d6067fbe900",46), Label("523254f9-af29-4772-903a-fbb982ded0f2",58),
    Label("58ffea11-b686-4897-9153-1e2a8627184b",87), Label("3961a27d-1a47-4406-af62-1cc297f5beac",15),
    Label("2fed06d0-28cd-4c90-b239-bcb0b63f0cf1",96), Label("17df94c1-9f04-4921-a75c-084c3d18f33d",28),
    Label("95b603cf-df52-4024-8d60-b3b6804292cc",1), Label("e380cac9-0545-4d39-beb7-8b9cec873298",34),
    Label("6e316305-ce66-4fdf-8c72-85ad13c5cef8",60), Label("35b39859-a2b8-4bbd-9d30-e3e549b6a1d8",1),
    Label("c2a33d97-d657-4732-9466-c23ed6e0b0b3",99), Label("f6fbed1b-b54b-4b66-be28-1eea400b3f97",95),
    Label("710f8425-7d23-4730-b8b2-baa53aca8317",33), Label("a299a3ed-4be6-4847-abea-b508a07cc333",61),
    Label("694822bf-0131-4a1f-8a92-4223f77b18d8",39), Label("9e4ce486-59a2-44ad-bd4e-570bdaf84398",56),
    Label("2c6b05c5-86ec-4e3f-8d9e-f88ed32106a0",47), Label("97866f87-415a-456b-9336-ca8ca4a8fd9d",49),
    Label("a3ce57aa-7600-4e86-bdff-a7f72d43cf78",96), Label("317f4f56-e0e7-4cae-97ec-78cfa0e6ecfa",54),
    Label("91247877-4a46-4d0b-bba8-235267cd92b0",16), Label("37e91e24-4d50-4a65-a053-39c6150a9808",69),
    Label("6de7aa7e-377a-428b-8f7d-2786e2328aa3",11), Label("256c431c-f69a-4858-b5c2-48a21085d9ff",97),
    Label("830c68bb-cc1f-4d8d-933e-b8a88a8d1b4e",90), Label("7bcdbe1a-539b-4cfc-b921-2186e72e6346",9),
    Label("623b7765-bc63-4e67-a013-012f7d28d577",90), Label("13041cf3-2ef2-4b1d-ba3d-b20c1cd59e3b",29),
    Label("5a70e377-5690-465c-ad3a-e81e8ac3db9a",42), Label("c4d36f32-599d-4dab-9058-ceb2d90c9039",100),
    Label("83856c4b-d5e0-4369-b008-cab5be47554e",9), Label("274e064e-594b-40ea-8e35-dbad2a40f673",82),
    Label("e513a600-1a23-417b-a425-80f4e3db556a",59), Label("38dec327-593f-4f6f-bc68-7f6517822e76",59),
    Label("97d4c7a3-5474-4f7c-8af1-327d661660a5",2), Label("cd2abdd4-de3a-4075-a5f4-4d1e8359ec36",4),
    Label("1e89bd20-973a-4f93-83f3-fff84a0ba985",26), Label("49bf44de-da5a-4a3b-912e-e463c32b1b56",87),
    Label("d42cc9f2-4f1c-40b3-94cd-1caa9601487c",35), Label("3eaec43d-1452-4096-9bb1-38eba0d7db52",26),
    Label("48d06923-42ad-4b95-a83f-54c3e02ae93e",37), Label("968111ff-720d-493e-8f35-1ba4ac33fa9b",99),
    Label("57889222-c999-4646-9ace-ed0ce18d12df",2), Label("dadb6520-c521-4b39-bb6b-01394fc7877a",64),
    Label("6039828a-f4d0-4e83-97f9-4b5b49407090",8), Label("e0ab397e-e01a-4242-aba5-74e547a08b5c",93),
    Label("addf536f-92c7-412a-b867-7f0d6869c75b",78), Label("9b5e2f96-a59f-42ea-b67d-6955ae727fc7",72),
    Label("aa088ab2-06e9-45d6-b9d3-a3b5f8e3fb4a",5), Label("b3bf622c-d6c2-4df4-bc05-300d2a6463bc",6),
    Label("5796574f-0acb-498f-87a4-2008746e7412",8), Label("efd19d35-0bf7-47d2-945e-6d5970af4dce",77),
    Label("cefde309-02cd-4920-a465-8d7c3be5a611",100), Label("a10d1a1e-0b51-42e5-9f1e-d800b58ba8a5",26),
    Label("6e25ccc4-5c1d-4d23-84c6-5057f7018559",28), Label("9ed7e86e-22e7-4759-9a60-5010c9ec7fca",12),
    Label("ae1e984f-ad73-4bc9-bff0-6268339164f9",44), Label("fb8486dc-351c-41b6-812e-e648286d22e3",50),
    Label("8ed44be9-5725-491d-a43e-9641d2680d90",9), Label("b6b642a1-4319-4007-afe5-f7c9aa342ed8",27)
  ), List(
    Label("91e48588-05c7-4b2d-b880-6daed1e016fa",38)~>Label("83856c4b-d5e0-4369-b008-cab5be47554e",9),
    Label("91e48588-05c7-4b2d-b880-6daed1e016fa",38)~>Label("fb8486dc-351c-41b6-812e-e648286d22e3",50),
    Label("523254f9-af29-4772-903a-fbb982ded0f2",58)~>Label("6e316305-ce66-4fdf-8c72-85ad13c5cef8",60),
    Label("523254f9-af29-4772-903a-fbb982ded0f2",58)~>Label("aa088ab2-06e9-45d6-b9d3-a3b5f8e3fb4a",5),
    Label("523254f9-af29-4772-903a-fbb982ded0f2",58)~>Label("e513a600-1a23-417b-a425-80f4e3db556a",59),
    Label("2fed06d0-28cd-4c90-b239-bcb0b63f0cf1",96)~>Label("968111ff-720d-493e-8f35-1ba4ac33fa9b",99),
    Label("2fed06d0-28cd-4c90-b239-bcb0b63f0cf1",96)~>Label("9b5e2f96-a59f-42ea-b67d-6955ae727fc7",72),
    Label("95b603cf-df52-4024-8d60-b3b6804292cc",1)~>Label("ae1e984f-ad73-4bc9-bff0-6268339164f9",44),
    Label("95b603cf-df52-4024-8d60-b3b6804292cc",1)~>Label("8ed44be9-5725-491d-a43e-9641d2680d90",9),
    Label("95b603cf-df52-4024-8d60-b3b6804292cc",1)~>Label("710f8425-7d23-4730-b8b2-baa53aca8317",33),
    Label("95b603cf-df52-4024-8d60-b3b6804292cc",1)~>Label("21365d6c-d0a3-4ecc-a5ed-5d6067fbe900",46),
    Label("6e316305-ce66-4fdf-8c72-85ad13c5cef8",60)~>Label("91e48588-05c7-4b2d-b880-6daed1e016fa",38),
    Label("6e316305-ce66-4fdf-8c72-85ad13c5cef8",60)~>Label("97866f87-415a-456b-9336-ca8ca4a8fd9d",49),
    Label("35b39859-a2b8-4bbd-9d30-e3e549b6a1d8",1)~>Label("6039828a-f4d0-4e83-97f9-4b5b49407090",8),
    Label("35b39859-a2b8-4bbd-9d30-e3e549b6a1d8",1)~>Label("58ffea11-b686-4897-9153-1e2a8627184b",87),
    Label("35b39859-a2b8-4bbd-9d30-e3e549b6a1d8",1)~>Label("3eaec43d-1452-4096-9bb1-38eba0d7db52",26),
    Label("35b39859-a2b8-4bbd-9d30-e3e549b6a1d8",1)~>Label("b3bf622c-d6c2-4df4-bc05-300d2a6463bc",6),
    Label("c2a33d97-d657-4732-9466-c23ed6e0b0b3",99)~>Label("61e8630d-74e0-4585-aecb-09f4bd19c851",95),
    Label("c2a33d97-d657-4732-9466-c23ed6e0b0b3",99)~>Label("c4d36f32-599d-4dab-9058-ceb2d90c9039",100),
    Label("c2a33d97-d657-4732-9466-c23ed6e0b0b3",99)~>Label("37e91e24-4d50-4a65-a053-39c6150a9808",69),
    Label("c2a33d97-d657-4732-9466-c23ed6e0b0b3",99)~>Label("e380cac9-0545-4d39-beb7-8b9cec873298",34),
    Label("317f4f56-e0e7-4cae-97ec-78cfa0e6ecfa",54)~>Label("5a70e377-5690-465c-ad3a-e81e8ac3db9a",42),
    Label("317f4f56-e0e7-4cae-97ec-78cfa0e6ecfa",54)~>Label("a3ce57aa-7600-4e86-bdff-a7f72d43cf78",96),
    Label("317f4f56-e0e7-4cae-97ec-78cfa0e6ecfa",54)~>Label("06cd64af-82b6-423f-9709-3b639f04a374",17),
    Label("38dec327-593f-4f6f-bc68-7f6517822e76",59)~>Label("f6fbed1b-b54b-4b66-be28-1eea400b3f97",95),
    Label("38dec327-593f-4f6f-bc68-7f6517822e76",59)~>Label("3961a27d-1a47-4406-af62-1cc297f5beac",15),
    Label("97d4c7a3-5474-4f7c-8af1-327d661660a5",2)~>Label("38dec327-593f-4f6f-bc68-7f6517822e76",59),
    Label("97d4c7a3-5474-4f7c-8af1-327d661660a5",2)~>Label("7bcdbe1a-539b-4cfc-b921-2186e72e6346",9),
    Label("cd2abdd4-de3a-4075-a5f4-4d1e8359ec36",4)~>Label("35b39859-a2b8-4bbd-9d30-e3e549b6a1d8",1),
    Label("cd2abdd4-de3a-4075-a5f4-4d1e8359ec36",4)~>Label("17df94c1-9f04-4921-a75c-084c3d18f33d",28),
    Label("cd2abdd4-de3a-4075-a5f4-4d1e8359ec36",4)~>Label("2c6b05c5-86ec-4e3f-8d9e-f88ed32106a0",47),
    Label("3eaec43d-1452-4096-9bb1-38eba0d7db52",26)~>Label("274e064e-594b-40ea-8e35-dbad2a40f673",82),
    Label("3eaec43d-1452-4096-9bb1-38eba0d7db52",26)~>Label("6de7aa7e-377a-428b-8f7d-2786e2328aa3",11),
    Label("3eaec43d-1452-4096-9bb1-38eba0d7db52",26)~>Label("dadb6520-c521-4b39-bb6b-01394fc7877a",64),
    Label("3eaec43d-1452-4096-9bb1-38eba0d7db52",26)~>Label("91247877-4a46-4d0b-bba8-235267cd92b0",16),
    Label("e0ab397e-e01a-4242-aba5-74e547a08b5c",93)~>Label("317f4f56-e0e7-4cae-97ec-78cfa0e6ecfa",54),
    Label("e0ab397e-e01a-4242-aba5-74e547a08b5c",93)~>Label("b6b642a1-4319-4007-afe5-f7c9aa342ed8",27),
    Label("e0ab397e-e01a-4242-aba5-74e547a08b5c",93)~>Label("c2a33d97-d657-4732-9466-c23ed6e0b0b3",99),
    Label("b3bf622c-d6c2-4df4-bc05-300d2a6463bc",6)~>Label("830c68bb-cc1f-4d8d-933e-b8a88a8d1b4e",90),
    Label("b3bf622c-d6c2-4df4-bc05-300d2a6463bc",6)~>Label("49bf44de-da5a-4a3b-912e-e463c32b1b56",87),
    Label("b3bf622c-d6c2-4df4-bc05-300d2a6463bc",6)~>Label("d42cc9f2-4f1c-40b3-94cd-1caa9601487c",35),
    Label("5796574f-0acb-498f-87a4-2008746e7412",8)~>Label("78a77011-d93b-4f31-9a0e-f8f247b62f64",26),
    Label("5796574f-0acb-498f-87a4-2008746e7412",8)~>Label("1e89bd20-973a-4f93-83f3-fff84a0ba985",26),
    Label("5796574f-0acb-498f-87a4-2008746e7412",8)~>Label("2fed06d0-28cd-4c90-b239-bcb0b63f0cf1",96),
    Label("cefde309-02cd-4920-a465-8d7c3be5a611",100)~>Label("95b603cf-df52-4024-8d60-b3b6804292cc",1),
    Label("cefde309-02cd-4920-a465-8d7c3be5a611",100)~>Label("e0ab397e-e01a-4242-aba5-74e547a08b5c",93),
    Label("a10d1a1e-0b51-42e5-9f1e-d800b58ba8a5",26)~>Label("cd2abdd4-de3a-4075-a5f4-4d1e8359ec36",4),
    Label("a10d1a1e-0b51-42e5-9f1e-d800b58ba8a5",26)~>Label("6e25ccc4-5c1d-4d23-84c6-5057f7018559",28),
    Label("a10d1a1e-0b51-42e5-9f1e-d800b58ba8a5",26)~>Label("523254f9-af29-4772-903a-fbb982ded0f2",58),
    Label("a10d1a1e-0b51-42e5-9f1e-d800b58ba8a5",26)~>Label("cefde309-02cd-4920-a465-8d7c3be5a611",100),
    Label("6e25ccc4-5c1d-4d23-84c6-5057f7018559",28)~>Label("5796574f-0acb-498f-87a4-2008746e7412",8),
    Label("6e25ccc4-5c1d-4d23-84c6-5057f7018559",28)~>Label("97d4c7a3-5474-4f7c-8af1-327d661660a5",2),
    Label("ae1e984f-ad73-4bc9-bff0-6268339164f9",44)~>Label("a299a3ed-4be6-4847-abea-b508a07cc333",61),
    Label("ae1e984f-ad73-4bc9-bff0-6268339164f9",44)~>Label("57889222-c999-4646-9ace-ed0ce18d12df",2),
    Label("ae1e984f-ad73-4bc9-bff0-6268339164f9",44)~>Label("addf536f-92c7-412a-b867-7f0d6869c75b",78),
    Label("8ed44be9-5725-491d-a43e-9641d2680d90",9)~>Label("48d06923-42ad-4b95-a83f-54c3e02ae93e",37),
    Label("8ed44be9-5725-491d-a43e-9641d2680d90",9)~>Label("efd19d35-0bf7-47d2-945e-6d5970af4dce",77),
    Label("8ed44be9-5725-491d-a43e-9641d2680d90",9)~>Label("13041cf3-2ef2-4b1d-ba3d-b20c1cd59e3b",29),
    Label("8ed44be9-5725-491d-a43e-9641d2680d90",9)~>Label("9ed7e86e-22e7-4759-9a60-5010c9ec7fca",12),
    Label("b6b642a1-4319-4007-afe5-f7c9aa342ed8",27)~>Label("623b7765-bc63-4e67-a013-012f7d28d577",90),
    Label("b6b642a1-4319-4007-afe5-f7c9aa342ed8",27)~>Label("694822bf-0131-4a1f-8a92-4223f77b18d8",39),
    Label("b6b642a1-4319-4007-afe5-f7c9aa342ed8",27)~>Label("9e4ce486-59a2-44ad-bd4e-570bdaf84398",56),
    Label("b6b642a1-4319-4007-afe5-f7c9aa342ed8",27)~>Label("256c431c-f69a-4858-b5c2-48a21085d9ff",97)
  ))


  val (p, q) = (2,3)

  "labeltuple union" should "takes the min-union" in {
    val ops = PqExtended
    val p1 = ops.apply(arg0, p, q).profile
    val p2 = ops.apply(arg1, p, q).profile
    p1.distance(p2) shouldEqual p2.distance(p1)
  }
}
